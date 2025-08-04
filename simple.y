/*
 * simple.y: simple parser for the simple "C" language
 */

%token <string_val> WORD

%token  NOTOKEN LPARENT RPARENT LBRACE RBRACE LCURLY RCURLY COMA SEMICOLON EQUAL STRING_CONST LONG LONGSTAR VOID CHARSTAR CHARSTARSTAR INTEGER_CONST AMPERSAND OROR ANDAND EQUALEQUAL NOTEQUAL LESS GREAT LESSEQUAL GREATEQUAL PLUS MINUS TIMES DIVIDE PERCENT IF ELSE WHILE DO FOR CONTINUE BREAK RETURN SWITCH CASE DEFAULT COLON

%union  {
  char   *string_val;
  int nargs;
  int my_nlabel;

  }


%{
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int yylex();
int yyerror(const char * s);

extern int line_number;
const char * input_file;
char * asm_file;
FILE * fasm;

#define MAX_ARGS 5
int nargs;
char * args_table[MAX_ARGS];

#define MAX_GLOBALS 100
int nglobals = 0;
char * global_vars_table[MAX_GLOBALS];

#define MAX_LOCALS 32
int nlocals = 0;
char *local_vars_table[MAX_LOCALS];

#define MAX_STRINGS 100
int nstrings = 0;
char * string_table[MAX_STRINGS];

char *regStk[]={ "rbx", "r10", "r13", "r14", "r15"};
char nregStk = sizeof(regStk)/sizeof(char*);

char *regArgs[]={ "rdi", "rsi", "rdx", "rcx", "r8", "r9"};
char nregArgs = sizeof(regArgs)/sizeof(char*);


char *s_count_table[MAX_GLOBALS];
int string_variable_count = 0;

int type_of_variable = -1;

int top = 0;

int nargs =0;
 
int nlabel = 0;

/*
 * Variable to keep track of what loop for break
 * 0 = none
 * 1 = if
 * 2 = for
 * 3 = while
 * 4 = do- while
 * 5 - Switch
 */

int looptype = 0;

int currentLabel = 0;



/*
 * Compiler Implementation Overview
 * ================================
 *
 * Variable Management
 * -------------------
 * 1. Local Variables:
 *    - Kept track of in 'local_vars_table'
 *    - Stored on the stackat negative offsets from the base ptr (rbp)
 *    - Managed through 'nlocals' varaibles
 *
 * 2. Global Variables:
 *    - Kept track of in the 'global_vars_table'
 *    - Declared in .data section
 *    - Uses .comm to reserve 8 bytes of memory (not initialized)
 *
 * Type Handling
 * -------------
 * Variables are kept track of using 'type_of_variable'
 *    - CHAR*
 *    - CHAR**
 *    - LONG
 *    - LONG*
 *    - VOID  
 *
 * Array Handling
 * --------------
 * 1. String arrays:
 *    - String arrays are handled with 'string_variable_count' and 's_count_table'
 *    - Handles local and global variables
 *    - Uses byte level indexing for character access
 *
 * 2. Number arrays:
 *    - Calculates base address and applies the offset to base ptr (rbp)
 *    - Handles local and global variables
 *    - Uses scaling (multiplies index by 8 to maintain proper memory layout)
 *
 * Expression Handling
 * -------------------
 * 1. Logical Expression:
 *    - Implements AND (&&) and OR (||)
 *    - Usees comparisona and jumps to evaluate the conditions
 *    - Generates unique lables for each logical expression
 *
 * 2. Comparison Expression:
 *    - Is able to do ==, !=, <, >, <=, >=
 *    - Uses comparison and set instructions (sete, setne, ect.)
 *    - Stored as 0 (false) or 1 (true)
 *
 *    * I believe I used "short circuitting" for this part ?
 *
 * Control Flow Statements
 * -----------------------
 * 1. If-Else Statements:
 *    - Uses unique labels for each if block (if_start, if_else, if_end)
 *    - Generates conditional jumps based on the expression evaluation
 *    - Can handle the else clause as well
 *    - Condition is checked by comparing top register (rbx) to 0
 *
 * 2. While Loops:
 *    - Generates unique label for each loop start
 *    - Codition is check at beginning 
 *    - Jumps to end if the condition is false
 *    - Supports break and continue statements as well
 *
 * 3. Do-While loops:
 *    - Unique labels for loop start and end
 *    - Condition is check at the end of each iteration
 *    - Supports break and continue statements
 *
 * 4. For Loops:
 *    - Generates multiple labels
 *      * for_start: Loop condition check
 *      * for_increment: Incremenet section
 *      * for_body: Main loop body
 *      * for_end: Ends the loop
 *    - Supports the intialiaztion, condtion and increment expression
 *    - Suppporst break and acontinue statements as well
 *
 * Jump Statements
 * ---------------
 * 1. Break Statement:
 *    - Jumps to the end of the current loop
 *    - works with for, while and do-whlie loops
 *    - Uses 'looptype' to determine the correct jump destination
 *    - Uses 'currentLabel' to determine what the correct loop label to break out of should be
 *
 * 2. Continue Statement:
 *    - Jumps to the start/increment part of the current loop
 *    - Works with for, while, do-while loops
 *    - Uses 'looptype' to determine the correct jump destination
 *    - Uses 'currentLabel' to determine what the correct loop label to break out of should be
 *
 * Loop Tracking
 * -------------
 * 'looptype' variable tracks the current loop type
 *    - 0: No loop
 *    - 2: For loop
 *    - 3: While loop
 *    - 4: Do-while loop
 *
 * `currentLabel` stores the unique label for the innermost loop
 * This enables proper handling of nested loops and jump statements
 *
 * Label Generation
 * ----------------
 * - Uses `nlabel` to generate unique, incrementing labels
 * - Ensures no label conflicts in complex nested structures
 * - Labels follow a clear naming convention:
 *   * if_start_X, if_else_X, if_end_X
 *   * while_start_X, while_end_X
 *   * do_while_start_X, do_end_X
 *   * for_start_X, for_increment_X, for_body_X, for_end_X
 *
 *
 *
 *
 *
 *
 *
 *
 */


%}

%%

goal:	program
  ;

program :
        function_or_var_list;

function_or_var_list:
        function_or_var_list function
        | function_or_var_list global_var
        | /*empty */
	;

function:
         var_type WORD
         {
            nlocals = 0;

            nargs = 0; // Reset argument counter at start of funciton


            fprintf(fasm, "\t.text\n");

            fprintf(fasm, ".globl %s\n", $2);

            fprintf(fasm, "%s:\n", $2);

            fprintf(fasm, "\t# Save Frame pointer\n");

            fprintf(fasm, "\tpushq %%rbp\n");

            fprintf(fasm, "\tmovq %%rsp, %%rbp\n");

            fprintf(fasm, "# Save registers. \n");

            fprintf(fasm, "# Push one extra to align stack to 16bytes\n");

            fprintf(fasm, "\tsubq $%d, %%rsp\n", (MAX_LOCALS*8));

            fprintf(fasm, "\tpushq %%rbx\n");
            fprintf(fasm, "\tpushq %%rbx\n");

            fprintf(fasm, "\tpushq %%r10\n");
            fprintf(fasm, "\tpushq %%r13\n");
            fprintf(fasm, "\tpushq %%r14\n");
            fprintf(fasm, "\tpushq %%r15\n");


          }
   LPARENT arguments RPARENT compound_statement
         {

            fprintf(fasm, "\taddq $%d, %%rsp\n", MAX_LOCALS*8);
            fprintf(fasm, "# Restore registers\n");
            fprintf(fasm, "\tpopq %%r15\n");
            fprintf(fasm, "\tpopq %%r14\n");
            fprintf(fasm, "\tpopq %%r13\n");
            fprintf(fasm, "\tpopq %%r10\n");
            fprintf(fasm, "\tpopq %%rbx\n");
            fprintf(fasm, "\tpopq %%rbx\n");

            fprintf(fasm, "\tleave\n");
            fprintf(fasm, "\tret\n");

         }
  ;

arg_list:

        arg {

          nargs = 1;
          args_table[0] = $<string_val>1; // Store the first arguement

         }
         | arg_list COMA arg {

          // Add arguements to the arg table if there is space
          if (nargs < MAX_ARGS) {
            args_table[nargs] = $<string_val>3;
            nargs++;
          }
         }
;

arguments:
         arg_list
   | /*empty*/ { nargs = 0;}
   ;




arg: var_type WORD {

      // Make space for local variable for argument
      assert(nlocals < MAX_LOCALS);
      local_vars_table[nlocals] = $2; // Store arguemnt name as local variable

      // Move arguement value to thje right spot on the stack
      fprintf(fasm, "\tmovq %%%s, -%d(%%rbp)\n", regArgs[nlocals], 8*(nlocals+1));
      nlocals++;
      $<string_val>$ = $2;


      // handle string variables
      if (type_of_variable == 1) {
        s_count_table[string_variable_count] = $<string_val>2;
        string_variable_count++;
      }

      }
      ;

global_var: 
        var_type global_var_list SEMICOLON;

global_var_list: WORD {

                  // Declars a global variable
                  char * id = $1;
                  fprintf(fasm, "       # global id=%s\n", id);
                  fprintf(fasm, "       .data\n");
                  fprintf(fasm, "       .comm %s,8\n", id);
                  fprintf(fasm, "\n");

                  // Handle string variables
                  if (type_of_variable == 1) {
                    s_count_table[string_variable_count] = $1;
                    string_variable_count++;
                  }

        }
        | global_var_list COMA WORD {

                  // Declares many global variables
                  assert(nglobals < MAX_GLOBALS);

                  global_vars_table[nglobals++] = $<string_val>1;

                  char *id = $3;
                  fprintf(fasm, "\t#global id = %s\n", id);
                  fprintf(fasm, "\t.data\n");
                  fprintf(fasm, "\t.comm %s, 8\n", id);
                  fprintf(fasm, "\n");

        }
        ;

var_type: 
      CHARSTAR {

          // variable type is CHAR*
          type_of_variable = 1;

      }
      | CHARSTARSTAR {

          // variable type is CHAR**
          type_of_variable = 2;

      }
      | LONG {

        // variable type is LONG
        type_of_variable = 3;

      }
      | LONGSTAR {

        // variable type is LONG*
        type_of_variable = 4;

      }
      | VOID {

        // variable type is VOID
        type_of_variable = 5;

      }
      ;

assignment:
         WORD EQUAL expression {

            // Find variable in local or global scope
            char * id = $<string_val>1;

            int localvar = -1;

            for (int i = 0; i < nlocals; i++) {
              if (strcmp(id, local_vars_table[i]) == 0) {
                localvar = i;
                break;
              }
            }

            if (localvar != -1) {
              // Local var
              fprintf(fasm, "\tmovq %%rbx, -%d(%%rbp)\n", 8*(localvar+1));

            }
            else {
              // Global var
              fprintf(fasm, "\tmovq %%rbx, %s\n", id);
            }

            top--;


         }

         // I BELIEVE THIS IS WHERE ARRAY INDEXING IS HANDLED //

   | WORD LBRACE expression RBRACE EQUAL expression {

    int check_if_string = 0; // Flag to cehck if varaible is a string

    for (int i = 0; i < string_variable_count; i++) {
      if (strcmp($1, s_count_table[i]) == 0) {
        check_if_string = 1;
        break;
      }
    }

    /* Have to handle string and non string arrays differently
     * Strings are treated with 1 byte per character
     * Strings have a null byte
     */
    if (check_if_string) {
      // STRING ARRAY HANDLING:
      // - Use byte level operations (movb instead of movq)
      // - Allows for character by character manipulation
      // Treat each element as a single byte

      int local_var = -1;
      char *var = $1;

      // Search for variable in tables
      for (int i = 0; i < nlocals; i++) {
        if (strcmp(local_vars_table[i], var) == 0) {
          local_var = i;
          break;
        }
      }

      if (local_var == -1) {
        // GLOBAL VARIABLE
        // Add address of global variable to the register
        fprintf(fasm, "\taddq %s, %%%s\n", $1, regStk[top-2]);
      }
      else {
        // LOCAL VARIABLE
        // Multiply 8 to account for the 64 bit stack frame
        // Subtract offset to find the locatino of the local on the stack
        fprintf(fasm, "\taddq -%d(%%rbp), %%%s\n", 8*(local_var+1), regStk[top-2]);
      }

      // Move the value to be assigned to rax
      // Prepare value for storing in array
      fprintf(fasm, "\tmovq %%%s, %%rax\n", regStk[top-1]);

      // Move the lowest byte of value to memory locatoin
      fprintf(fasm, "\tmovb %%al, (%%%s)\n", regStk[top-2]);

      // Minus two because two registersed were used for array assignment
      // One used for array index calculation
      // Another used for value being assigned
      top -= 2;
    }
    else {
      // Numeric ARRAY handlnig
      // - Scales index by element size (8 bytes)
      int local_var = -1;
      char *var = $1;

      // Chekc if varaible is a local variable or not
      for (int i = 0; i < nlocals; i++) {
        if (strcmp(local_vars_table[i], var) == 0) {
          local_var = i;
          break;
        }
      }

      // Scale the index by 8 to maintain stack frame (correct offset)
      fprintf(fasm, "\timulq $8, %%%s\n", regStk[top-2]);

      if (local_var == -1) {
        // Global variable
        // Add memory address of the global array to register
        fprintf(fasm, "\taddq %s, %%%s\n", $1, regStk[top-2]);
      }
      else {
        // Local variable
        // calculate address of local array relatice to base ptr (rbp)
        // Subtracts offset to get location (stack grows down)
        fprintf(fasm, "\taddq -%d(%%rbp), %%%s\n", 8*(local_var+1), regStk[top-2]);
      }

      // Sotre value at array memory location
      fprintf(fasm, "\tmovq %%%s, (%%%s)\n", regStk[top-1], regStk[top-2]);

      // Same as String handling section
      top -= 2;
    }
}


    ;

call :
         WORD LPARENT  call_arguments RPARENT {

            char *funcName = $<string_val>1;
            int nargs = $<nargs>3;
            int i;
            fprintf(fasm,"     # func=%s nargs=%d\n", funcName, nargs);
            fprintf(fasm,"     # Move values from reg stack to reg args\n");

            // move args form register stack to the argument registers
            for (i = nargs - 1; i >= 0; i--) {
              top--;
              fprintf(fasm, "\tmovq %%%s, %%%s\n",
                  regStk[top], regArgs[i]);
            }

            if (!strcmp(funcName, "printf")) {
              // printf has a variable number of arguments
              // and it need the following
              fprintf(fasm, "\tmovl $0, %%eax\n");
            }

            // Call whatever function
            fprintf(fasm, "\tcall %s\n", funcName);

            // Store return value in register stack
            fprintf(fasm, "\tmovq %%rax, %%%s\n", regStk[top]);
            top++;
          }
          ;

call_arg_list:
         expression {
           $<nargs>$=1;
           }
         | call_arg_list COMA expression {
            $<nargs>$++;
         }

         ;

call_arguments:
         call_arg_list { $<nargs>$=$<nargs>1; }
   | /*empty*/ { $<nargs>$=0;}
   ;

expression :
         logical_or_expr
   ;



/*
 * the results for the logic expressions,
 * are move to regStk[top-2] to replace 2,
 * operands used in the comparison. This,
 * keeps intermediate results on the stack
 */
logical_or_expr:
         logical_and_expr
   | logical_or_expr OROR logical_and_expr {

    // Check to see if 1st opeand is not zero
    fprintf(fasm, "\tcmpq $0, %%%s\n", regStk[top-2]);
    fprintf(fasm, "\tjne true%d\n", nlabel); // jmp to end (only one has to be 1 for answer to be 1)

    // Check 2nd operand for same case if 1st is 0
    fprintf(fasm, "\tcmpq $0, %%%s\n", regStk[top-1]);
    fprintf(fasm, "\tjne true%d\n", nlabel);

    // Both must be zero if this point is reached
    fprintf(fasm, "\tmovq $0, %%%s\n", regStk[top-2]);
    fprintf(fasm, "\tjmp false%d\n", nlabel);

    // One is not zero answer is 1
    fprintf(fasm, "true%d:\n", nlabel);
    fprintf(fasm, "\tmovq $1, %%%s\n", regStk[top-2]);

    // False jump statement
    fprintf(fasm, "false%d:\n", nlabel);

    //reduce top of register stack
    top--;

    // increment nlabel
    /*
     * nlabel is a var used for unique labels
     * every time there is a new label,
     * nlabel must be incremented
     */

     nlabel++;

   }
   ;

logical_and_expr:
         equality_expr
   | logical_and_expr ANDAND equality_expr {

    // Check if first operand is not zero
    fprintf(fasm, "\tcmpq $0, %%%s\n", regStk[top-2]);
    fprintf(fasm, "\tsetne %%al\n"); // set al to 1 if not zero

    // if first is zero the answer must also be 0
    fprintf(fasm, "\tje zero%d\n", nlabel);

    // Check 2nd operandfasd
    fprintf(fasm, "\tcmpq $0, %%%s\n", regStk[top-1]);
    fprintf(fasm, "\tsetne %%al\n");

    // if 2nd is zero it must be 0
    fprintf(fasm, "\tje zero%d\n", nlabel);

    // Both are not 0 answer is 1
    fprintf(fasm, "\tmovq $1, %%%s\n", regStk[top-2]);
    fprintf(fasm, "\tjmp end%d\n", nlabel);

    // Zero jumpst statement
    fprintf(fasm, "zero%d:\n", nlabel);
    fprintf(fasm, "\tmovq $0, %%%s\n", regStk[top-2]);

    // End jumpst statment
    fprintf(fasm, "end%d:\n", nlabel);

    // reduce register stack
    top--;

    nlabel++;

   }

   ;

equality_expr:
         relational_expr
   | equality_expr EQUALEQUAL relational_expr {

      // Clear return register (rax)
      fprintf(fasm, "\tmovq $0, %%rax\n");

      // Compare top two values on the stack
      fprintf(fasm, "\tcmpq %%%s, %%%s\n", regStk[top-1], regStk[top-2]);

      // Set al to 1 if equal  0 otherwise
      fprintf(fasm, "\tsete %%al\n");

      // Move result to the top-most stack location
      fprintf(fasm, "\tmovq %%rax, %%%s\n", regStk[top-2]);
      top--;
    }
   | equality_expr NOTEQUAL relational_expr {

      // Clear the return register (rax)
      fprintf(fasm, "\tmovq $0, %%rax\n");

      // Compare the top two values on the stack
      fprintf(fasm, "\tcmpq %%%s, %%%s\n", regStk[top-1], regStk[top-2]);

      // Set al to 1 if not equal, 0 otherwise
      fprintf(fasm, "\tsetne %%al\n");

      // Move result to top-most stack location
      fprintf(fasm, "\tmovq %%rax, %%%s\n", regStk[top-2]);
      top--;
   }
   ;
relational_expr:
         additive_expr
   | relational_expr LESS additive_expr {

      // Clear return register (rax)
      fprintf(fasm, "\tmovq $0, %%rax\n");

      // Compare the top two values on the stack
      fprintf(fasm, "\tcmpq %%%s, %%%s\n", regStk[top-1], regStk[top-2]);

      // set al to 2 if less than  otherwise 0
      fprintf(fasm, "\tsetl %%al\n");

      // Move result to top-most register
      fprintf(fasm, "\tmovq %%rax, %%%s\n", regStk[top-2]);
      top--;
    }
   | relational_expr GREAT additive_expr {

      // Clear return register (rax)
      fprintf(fasm, "\tmovq $0, %%rax\n");

      // Compare top two values on the stack
      fprintf(fasm, "\tcmpq %%%s, %%%s\n", regStk[top-1], regStk[top-2]);

      // Set al to 1 if greater 0 otherwise
      fprintf(fasm, "\tsetg %%al\n");

      // Move result ot top-most register
      fprintf(fasm, "\tmovq %%rax, %%%s\n", regStk[top-2]);
      top--;
   }

   | relational_expr LESSEQUAL additive_expr {

      // Clear return register (rax)
      fprintf(fasm, "\tmovq $0, %%rax\n");

      // Compare top two values on the stack
      fprintf(fasm, "\tcmpq %%%s, %%%s\n", regStk[top-1], regStk[top-2]);

      // Set al to 1 if less than 0 otherwise
      fprintf(fasm, "\tsetle %%al\n");

      // Move result to top-most register
      fprintf(fasm, "\tmovq %%rax, %%%s\n", regStk[top-2]);
      top--;
    }

   | relational_expr GREATEQUAL additive_expr {

      // Clear return register (rax)
      fprintf(fasm, "\tmovq $0, %%rax\n");

      // Compare top two values on the stack
      fprintf(fasm, "\tcmpq %%%s, %%%s\n", regStk[top-1], regStk[top-2]);

      // Set al to 1 if greater than 0 otherwise
      fprintf(fasm, "\tsetge %%al\n");

      // Move result to top-most register
      fprintf(fasm, "\tmovq %%rax, %%%s\n", regStk[top-2]);
      top--;
  }
  ;

additive_expr:
          multiplicative_expr
    | additive_expr PLUS multiplicative_expr {

        // Hanldes addition
        // addq
        fprintf(fasm,"\n\t# +\n");
        if (top<nregStk) {
          fprintf(fasm, "\taddq %%%s,%%%s\n", 
                  regStk[top-1], regStk[top-2]);
          top--;
        }
    }
    | additive_expr MINUS multiplicative_expr {

        // Handles subtraction
        // subq
        fprintf(fasm,"\n\t# -\n");
        if (top<nregStk) {
          fprintf(fasm, "\tsubq %%%s,%%%s\n",
                  regStk[top-1], regStk[top-2]);
          top--;
        }
    }
    ;

multiplicative_expr:
          primary_expr
      | multiplicative_expr TIMES primary_expr {

          // Handles multiplication
          // imulq
          fprintf(fasm,"\n\t# *\n");
          if (top<nregStk) {
            fprintf(fasm, "\timulq %%%s,%%%s\n", 
                    regStk[top-1], regStk[top-2]);
            top--;
          } else {

          // CHECK IF NEED TO THROW ERROR (OUT OF REGISTERS)
          fprintf(stderr, "OUT OF REGISTERS\n");
          exit(1);
          }
      }
      | multiplicative_expr DIVIDE primary_expr {

          // Hanldes division
          // cqto --> idivq
          fprintf(fasm,"\n\t# /\n");
          if (top<nregStk) {
            fprintf(fasm, "\tmovq %%%s, %%rax\n", regStk[top-2]);

            // Sign extend into rax, rdx:rax
            fprintf(fasm, "\tcqto\n");  // Sign extend rax into rdx:rax

            // Do signed division
            fprintf(fasm, "\tidivq %%%s\n", regStk[top-1]);

            // Move quotient from rax to top of stack
            fprintf(fasm, "\tmovq %%rax, %%%s\n", regStk[top-2]);
            top--;
          }
     }
     ;
    | multiplicative_expr PERCENT primary_expr {

          // Handles Modulus
          // cqto --> idivq
          fprintf(fasm,"\n\t# %%\n");
          if (top<nregStk) {
            fprintf(fasm, "\tmovq %%%s, %%rax\n", regStk[top-2]);

            // Sign extend into rax, rdx:rax
            fprintf(fasm, "\tcqto\n");

            // do signed division
            fprintf(fasm, "\tidivq %%%s\n", regStk[top-1]);

            // Move the remainder from rdx to top of stack
            fprintf(fasm, "\tmovq %%rdx, %%%s\n", regStk[top-2]);
            top--;
          }
    }
    ;

primary_expr:
    STRING_CONST {
      // Add string to string table.
      // String table will be produced later
      string_table[nstrings]=$<string_val>1;
      fprintf(fasm, "\t#top=%d\n", top);
      fprintf(fasm, "\n\t# push string %s top=%d\n",
        $<string_val>1, top);
      if (top<nregStk) {
        fprintf(fasm, "\tmovq $string%d, %%%s\n", 
        nstrings, regStk[top]);
      //fprintf(fasm, "\tmovq $%s,%%%s\n", 
      //$<string_val>1, regStk[top]);
      top++;
      }
      nstrings++;
    }
    | call
    | WORD {

      // ID may be a local var or glabl var, See if it is local
      char * id = $<string_val>1;

      int localvar = -1;
      for (int i = 0; i < nlocals; i++) {
        if (strcmp(id, local_vars_table[i]) == 0) {
          localvar = i;
          break;
        }
      }

      if (localvar != -1) {
        // Local var
        fprintf(fasm, "\tmovq -%d(%%rbp), %%%s\n", 8*(localvar+1), regStk[top]);

      }
      else {
        // Global var
        fprintf(fasm, "\tmovq %s, %%%s\n", id, regStk[top]);

      }

      top++;
    }
    | WORD LBRACE expression RBRACE { // ARRAY ?

      int check_if_string = 0; // flag variable

      // Chekc if variable is string or not
      for (int i = 0; i < string_variable_count; i++) {
        if (strcmp($1, s_count_table[i]) == 0) {
          check_if_string = 1;
          break;
        }
      }

      if (check_if_string) {
        // String handling
        int local_var = -1;
        char *variable = $1;

        // Search for variable in local vars table
        for (int i = 0; i < nlocals; i++) {
          if (strcmp(local_vars_table[i], variable) == 0) {
            local_var = i;
            break;
        }
      }

      // Handle array base address
      if (local_var == -1) {
        // GLoval variable
        // Calulate memory location for global string array
        fprintf(fasm, "\taddq %s, %%%s\n", $1, regStk[top - 1]);
      } else {
        // Local variable
        // Calculate address with rbp
        // Subtract offset to find location on stack
        fprintf(fasm, "\taddq -%d(%%rbp), %%%s\n", 8 * (local_var + 1), regStk[top - 1]);
      }

      // Zero out the destination register
      fprintf(fasm, "\tmovq $0, %%%s\n", regStk[top]);

      // Load byte value and zero-extend
      // Conver single byte to 64-bit vaalue, fills upper bits with 0
      // For char[] want to load single character
      fprintf(fasm, "\tmovzbq (%%%s), %%%s\n", regStk[top - 1], regStk[top]);

      // Move result back to destination register
      // Makes sure loaded character is in the correct regsiter
      fprintf(fasm, "\tmovq %%%s, %%%s\n", regStk[top], regStk[top - 1]);

    } else {
      // Number array handling
      int local_var = -1;
      char *variable = $1;

      // Search for variable in local vars table
      for (int i = 0; i < nlocals; i++) {
        if (strcmp(local_vars_table[i], variable) == 0) {
            local_var = i;
            break;
        }
      }

      // Calculate array offset and load value
      fprintf(fasm, "\timulq $8, %%%s\n", regStk[top - 1]);

      if (local_var == -1) {
        // Global variable
        // Add glob. var address to register to find element address
        fprintf(fasm, "\taddq %s, %%%s\n", $1, regStk[top - 1]);

        // Load all bits to calcualated address
        // Get the entire numeric element
        fprintf(fasm, "\tmovq (%%%s), %%%s\n", regStk[top - 1], regStk[top - 1]);
      } else {
        // Locla variable
        // Calulcate address with rbp
        fprintf(fasm, "\taddq -%d(%%rbp), %%%s\n", 8 * (local_var + 1), regStk[top - 1]);

        // Load 64 bites to calulated address
        fprintf(fasm, "\tmovq (%%%s), %%%s\n", regStk[top - 1], regStk[top - 1]);
      }
    }
 }
    | AMPERSAND WORD {

      // Variable name
      char *var = $2;
      int local_var = -10;

      // Find variable in the local vars table
      for (int i = 0; i < nlocals; i++) {
        if (strcmp(var, local_vars_table[i]) == 0) {
          local_var = i;
          break;
        }
      }

      if (local_var < 0) {

        // Variable not in the local vars table
        // Global variable
        // Load memory address to register
        // Use $ to get address value
        fprintf(fasm, "\tmovq $%s, %%%s/n", var, regStk[top]);
      }
      else {

        // Variable found in the table
        // Local variable
        // load base ptr to register, starting point for calculating address
        fprintf(fasm, "\tmovq %%rbp, %%%s\n", regStk[top]);

        // Subtract offset to get address
        // x8 to maintain stack frame
        // get memory address of local variable relative to rbp
        fprintf(fasm, "\tsubq $%d, %%%s\n", 8*(local_var+1), regStk[top]);
      }

      // Incrmeent top of register stack
      top++;

    }
    | INTEGER_CONST {
        fprintf(fasm, "\n\t# push %s\n", $<string_val>1);
        if (top<nregStk) {
          fprintf(fasm, "\tmovq $%s,%%%s\n", 
                $<string_val>1, regStk[top]);
          top++;
        }
        else {
          fprintf(stderr, "Run out of registers\n");
          exit(1);
        }
      }
    | LPARENT expression RPARENT
    ;

compound_statement:
   LCURLY statement_list RCURLY
   ;

statement_list:
         statement_list statement
   | /*empty*/
   ;

local_var:
        var_type local_var_list SEMICOLON;

local_var_list:
          WORD {

            assert(nlocals < MAX_LOCALS);


            local_vars_table[nlocals] = $<string_val>1;
            //local_var_types[nlocals] = current_var_type; // Store current variable type

            if (type_of_variable == 1) {
              s_count_table[string_variable_count] = $1;
              string_variable_count++;
            }

            nlocals++;

          }
        | local_var_list COMA WORD {


          local_vars_table[nlocals] = $3;
          nlocals++;

        }
        ;

statement:
         assignment SEMICOLON
   | call SEMICOLON { top= 0; /* Reset register stack */ }
   | local_var
   | compound_statement
   | IF LPARENT {

      // Generate unique label for start of if block
      $<my_nlabel>1 = nlabel;
      nlabel++;
      fprintf(fasm, "if_start_%d:\n", $<my_nlabel>1);

    }
    expression RPARENT {

      // Check if condition is false or true
      fprintf(fasm, "\tcmpq $0, %%rbx\n");

      // If flase jump to the if_else_: section
      fprintf(fasm, "\tje if_else_%d\n", $<my_nlabel>1);
      top--;

    }
    statement {

      // Execute the statemnt and jump to the end
      fprintf(fasm, "\tjmp if_end_%d\n", $<my_nlabel>1);

      // Start of the else black
      fprintf(fasm, "if_else_%d:\n", $<my_nlabel>1);

    }
    else_optional {

      // end of the if statement
      fprintf(fasm, "if_end_%d:\n", $<my_nlabel>1);

    }
    | WHILE LPARENT {

      // generate unique label for the while loop
      $<my_nlabel>1 = nlabel;
      nlabel++;

      // Keep track of the current unique label for the loop
      // Used to dtermine the correct break statement for the correct loop
      currentLabel = $<my_nlabel>1;

      // Set looptype variable to 3
      // 3 corresponds to the while loop
      // This is used to determine the correct break statement
      looptype = 3;
      fprintf(fasm, "while_start_%d:\n", $<my_nlabel>1);

    }

    expression RPARENT {

      // Check if condition is false, if false jump to end of loop
      fprintf(fasm, "\tcmpq $0, %%rbx\n"); // 0 FALSE 1 TRUE
      fprintf(fasm, "\tje while_end_%d\n", $<my_nlabel>1);
      top--;
    }

    statement {

      // body of the loop: execute statement and then jump to start of loop
      fprintf(fasm, "\tjmp while_start_%d\n", $<my_nlabel>1);
      fprintf(fasm, "while_end_%d:\n", $<my_nlabel>1);

   }
   | DO {

      // Geneate unqiue label for do-while loop
      $<my_nlabel>1 = nlabel;
      nlabel++;

      // Keep track of current unique label
      // Used to determine the correct break statement point
      currentLabel = $<my_nlabel>1;

      // Set looptype variable to 4
      // 4 corresponds to do-while loop
      // Used to determine break statement for correct loop
      looptype = 4;

      fprintf(fasm, "do_while_start_%d:\n", $<my_nlabel>1);

   }
   statement WHILE LPARENT expression {

      // Check if conditino is false, if false jump to end of loop
      fprintf(fasm, "\tcmpq $0, %%%s\n", regStk[top-1]);
      fprintf(fasm, "\tje do_end_%d\n", $<my_nlabel>1);

      // jump to beginning of the loop
      fprintf(fasm, "\tjmp do_while_start_%d\n", $<my_nlabel>1);
      top--;

   }
   RPARENT SEMICOLON {

      // End of do-while loop
      fprintf(fasm, "do_end_%d:\n", $<my_nlabel>1);

   }
   | FOR LPARENT assignment {

      // Generate unique label for for loop
      $<my_nlabel>1 = nlabel;
      nlabel++;

      // Keep track of unique loop label
      currentLabel = $<my_nlabel>1;

      // Set looptype to 2 to track which loop the code is in
      looptype = 2;
      fprintf(fasm, "for_start_%d:\n", $<my_nlabel>1);

   }
   SEMICOLON expression SEMICOLON {

      // Check if conditino is false, if false jump to end of loop
      fprintf(fasm, "\tcmpq $0, %%%s\n", regStk[top-1]);
      fprintf(fasm, "\tje for_end_%d\n", $<my_nlabel>1);

      // Jump to the loop body
      fprintf(fasm, "\tjmp for_body_%d\n", $<my_nlabel>1);
      fprintf(fasm, "for_increment_%d:\n", $<my_nlabel>1);
      top--;
    }
   assignment RPARENT {

      // Increment/Decrement loop counter
      // Jump back to start of loop
      fprintf(fasm, "\tjmp for_start_%d\n", $<my_nlabel>1);
      fprintf(fasm, "for_body_%d:\n", $<my_nlabel>1);

   }
   statement {

      // Loop body: exeutes the statement and jumps to increment/decrement section
      fprintf(fasm, "\tjmp for_increment_%d\n", $<my_nlabel>1);
      fprintf(fasm, "for_end_%d:\n", $<my_nlabel>1);
   }
   | SWITCH LPARENT expression {
      $<my_nlabel>1 = nlabel;
      nlabel++;

      currentLabel = $<my_nlabel>1;

      looptype = 4;

      fprintf(fasm, "\tpushq %%rbx\n");





    }
    RPARENT LCURLY case_list {
      fprintf(fasm, "switch_end_%d:\n", $<my_nlabel>1);


      fprintf(fasm, "\tpopq %%rbx\n");
    } DEFAULT COLON statement BREAK SEMICOLON {



    } RCURLY;

    case_item:
      CASE expression COLON {
        $<my_nlabel>1 = nlabel;
        nlabel++;

        // Compare switch expression with case expression
        fprintf(fasm, "\tpopq %%rax\n");  // Restore switch expression
        fprintf(fasm, "\tpushq %%rax\n");  // Keep switch expression on stack
        fprintf(fasm, "\tcmpq %%rbx, %%rax\n");  // Compare switch value with case value
        fprintf(fasm, "\tjne case_next_%d\n", $<my_nlabel>1);  // Jump if not equal
      } statement BREAK SEMICOLON {

              fprintf(fasm, "\tjmp switch_end_%d\n", currentLabel);

        // Label for next case if this one doesn't match
        fprintf(fasm, "\tcase_next_%d:\n", $<my_nlabel>1);
      }

    case_list:
      case_item /* at least one */
      | case_list case_item;


   | jump_statement
   ;

else_optional:
   ELSE statement
   | /* empty */
   ;

jump_statement:
         CONTINUE SEMICOLON {

          // Jump to beginning of the current loop
          switch(looptype) {
            case 2: // For loop
              fprintf(fasm, "\tjmp for_increment_%d\n", currentLabel);
              break;
            case 3: // while loop
              fprintf(fasm, "\tjmp while_start_%d\n", currentLabel);
              break;
           case 4: // Do-while loopp
              fprintf(fasm, "\tjmp do_while_start_%d\n", currentLabel);
              break;
            }
          }
   | BREAK SEMICOLON {

        // Jumps to end of current loop
        switch(looptype) {
          case 2: // For loop
            fprintf(fasm, "\tjmp for_end_%d\n", currentLabel);
             break;
          case 3: // While
            fprintf(fasm, "\tjmp while_end_%d\n", currentLabel);
            break;
         case 4: // Do-while loop
            fprintf(fasm, "\tjmp do_end_%d\n", currentLabel);
            break;
         }
        }


   | RETURN expression SEMICOLON {

      // Pop registers and do return stuff
      fprintf(fasm, "\tmovq %%rbx, %%rax\n");

      top = 0;

      fprintf(fasm, "\t # Restore registers\n");
      fprintf(fasm, "\tpopq %%r15\n");
      fprintf(fasm, "\tpopq %%r14\n");
      fprintf(fasm, "\tpopq %%r13\n");
      fprintf(fasm, "\tpopq %%r10\n");
      fprintf(fasm, "\tpopq %%rbx\n");
      fprintf(fasm, "\tpopq %%rbx\n");

      fprintf(fasm, "\taddq $%d, %%rsp\n", (MAX_LOCALS * 8));

      fprintf(fasm, "\tleave\n");
      fprintf(fasm, "\tret\n\n");
      fprintf(fasm, "\n");
   }
   ;

%%

void yyset_in (FILE *  in_str );

int
yyerror(const char * s)
{
	fprintf(stderr,"%s:%d: %s\n", input_file, line_number, s);
}


int
main(int argc, char **argv)
{
	printf("-------------WARNING: You need to implement global and local vars ------\n");
	printf("------------- or you may get problems with top------\n");
	
	// Make sure there are enough arguments
	if (argc <2) {
		fprintf(stderr, "Usage: scc file\n");
		exit(1);
	}

	// Get file name
	input_file = strdup(argv[1]);

	int len = strlen(input_file);
	if (len < 2 || input_file[len-2]!='.' || input_file[len-1]!='c') {
		fprintf(stderr, "Error: file extension is not .c\n");
		exit(1);
	}

	// Get assembly file name
	asm_file = strdup(input_file);
	asm_file[len-1]='s';

	// Open file to compile
	FILE * f = fopen(input_file, "r");
	if (f==NULL) {
		fprintf(stderr, "Cannot open file %s\n", input_file);
		perror("fopen");
		exit(1);
	}

	// Create assembly file
	fasm = fopen(asm_file, "w");
	if (fasm==NULL) {
		fprintf(stderr, "Cannot open file %s\n", asm_file);
		perror("fopen");
		exit(1);
	}

	// Uncomment for debugging
	//fasm = stderr;

	// Create compilation file
	// 
	yyset_in(f);
	yyparse();

	// Generate string table
	int i;
	for (i = 0; i<nstrings; i++) {
		fprintf(fasm, "string%d:\n", i);
		fprintf(fasm, "\t.string %s\n\n", string_table[i]);
	}

	fclose(f);
	fclose(fasm);

	return 0;
}


