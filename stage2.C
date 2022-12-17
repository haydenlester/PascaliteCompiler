// Hayden Lester
// CS4301
// Stage 2

// Received guidance from Womack for stage 1
// Womack assisted with explaining contentsOfAReg, temps, and pointed out a few edge cases I should be careful of

/*
Custom test cases I made helped a lot.
Those custom cases made it possible to unit test each production which sped everything up.
By the time I had finished, the only bugs that existed were in the emits.
This ensured I didn't need to do much bugfixing after-the-fact.

My thinking on this project was that if I was extra careful I could avoid plenty of time debugging.
I think the "slow and steady" mentality helped a lot.
I really enjoyed this project.
*/

#include <stage2.h>
#include <iostream>
#include <string>
#include <fstream>
#include <cctype>
#include <iomanip>
#include <ctime>
using namespace std;

Compiler::Compiler( char **argv){
   
    // open source file, listing file, object file
    this->sourceFile.open(argv[1]);
    this->listingFile.open(argv[2]);
    this->objectFile.open(argv[3]);
   
}

Compiler::~Compiler(){
   
   // close all files
   this->sourceFile.close();
   this->listingFile.close();
   this->objectFile.close();
   
}

void Compiler::createListingHeader(){
   
   time_t now = time (NULL);
   
   lineNo++;
   
   listingFile << "STAGE1:  " << setw(19) << left << "HAYDEN LESTER" << ctime(&now) << endl;
   listingFile << setw(22) << left << "LINE NO." << "SOURCE STATEMENT" << endl << endl;
   listingFile << right << setw(6) << to_string(lineNo) + "|";
   
}

void Compiler::parser(){
   
   nextChar();
   if (nextToken() != "program"){
      processError("keyword \"program\" expected");
   }
   prog();
   
}

void Compiler::createListingTrailer(){
   listingFile << endl << left << setw(27) << "COMPILATION TERMINATED      "
   << errorCount << " ERROR" << (errorCount == 1 ? "" : "S") << " ENCOUNTERED" << endl;
}

/* -----------
PRODUCTIONS
*/
void Compiler::prog(){
   if (token != "program")
      processError("keyword \"program\" expected");
   progStmt();
   if (token == "const")
      consts();
   if (token == "var")
      vars();
   if (token != "begin")
      processError("keyword \"begin\" expected");
   beginEndStmt();
   if(token != "$")
      processError("no text may follow \"end\" : " + token);
}

void Compiler::progStmt(){
   string x;
   if (token != "program")
      processError("keyword \"program\" expected");
   x = nextToken();
   if(!isNonKeyId(token))
      processError("program name expected");
   if(nextToken() != ";")
      processError("';' expected");
   nextToken();
   code("program", x);
   insert(x,PROG_NAME,CONSTANT,x,NO,0);
}

void Compiler::consts(){
	if(token != "const")
		processError("keyword \"const\" expected");
	if(!isNonKeyId(nextToken()))
		processError("non-keyword identifier must follow \"const\"");
	constStmts();
}

void Compiler::vars(){
   
   if(token != "var")
      processError("keyword \"var\" expected");
   if(!isNonKeyId(nextToken())){
      processError("non-keyword identifier must follow \"var\"");
   }
   varStmts();
   
}

void Compiler::beginEndStmt(){
	if (token != "begin")
		processError("keyword \"begin\" expected");
	if(isNonKeyId(nextToken()) || token == "read" || token == "write" || token == "if" || token == "while" || token == "repeat" || token == ";")
		execStmts();
	if(token != "end")
		processError("keywords \"read\", \"write\", \"end\", \"if\", \"while\", \"repeat\", \";\" or non keyword identifier expected. got: '" + token + "'");
	if(nextToken() != "." && token != ";")
		processError("period or semicolon expected");
   code("end", token);
   nextToken();
}

void Compiler::constStmts(){
   string x, y;
   if(!isNonKeyId(token))
      processError("non-keyword identifier expected");
   x = token;
   if(nextToken() != "=")
      processError("\"=\" expected");
   y = nextToken();
   if(!isNonKeyId(y) && !isInteger(y) && (y != "false") && (y != "true")
      && (y != "+") && (y != "-") && (y != "not"))
      processError("token to the right of \"=\" illegal");
   if(y == "+" || y == "-"){
      if(!isInteger(nextToken()))
         processError("integer expected after sign");
      y = y + token;
   }
   if(y == "not"){
      if(!isBoolean(nextToken()) && !isNonKeyId(token))
         processError("boolean literal or non keyword identifier expected after \"not\"");
      string z;
      if(isNonKeyId(token)){
         if(symbolTable.find(token) == symbolTable.end())
            processError("reference to undefined symbol '" + token + "'");
         z = symbolTable.at(token).getValue();
      }else
         z = token;
      if(token == "true")
         y = "false";
      else
         y = "true";
   }
   if(nextToken() != ";")
      processError("';' expected");
   if(whichType(y) != INTEGER && whichType(y) != BOOLEAN){
      processError("the data type of y is not INTEGER or BOOLEAN");
   }
   insert(x, whichType(y),CONSTANT,whichValue(y),YES,1);
   x = nextToken();
   if(x != "begin" && x != "var" && !isNonKeyId(x))
      processError("non-keyword identifier, \"begin\", or \"var\" expected");
   if(isNonKeyId(x)){
      constStmts();
   }
}

void Compiler::varStmts(){
   string x,y;
   if(!isNonKeyId(token))
      processError("non-keyword identifier expected");
   x = ids();
   if(token != ":")
      processError("\":\" expected");
   if(!(nextToken() == "boolean") && !(token == "integer"))
      processError("illegal type follows \":\"");
   y = token;
   if(nextToken() != ";")
      processError("';' expected");
   insert(x, y == "boolean" ? BOOLEAN : INTEGER, VARIABLE, "", YES, 1);
   if((nextToken() != "begin") && !isNonKeyId(token))
      processError("non-keyword identifier or \"begin\" expected");
   if(isNonKeyId(token))
      varStmts();
}

string Compiler::ids(){
   string temp, tempString;
   if(!isNonKeyId(token))
      processError("non-keyword identifier expected");
   tempString = token;
   temp = token;
   if(nextToken() == ","){
      if(!isNonKeyId(nextToken()))
         processError("non-keyword identifier expected");
      tempString = temp + "," + ids();
   }
   return tempString;
}

// Stage 1 Additions

void Compiler::execStmts(){
	
	if(!isNonKeyId(token) && token != "read" && token != "write"
      && token != "if" && token != "while" && token != "repeat"
      && token != ";" && token != "begin" && token != "end"
      && token != "until"){
		processError("execStmts: Invalid token. Got " + token);
	}
	
	if(token != "end" && token != "until"){
		execStmt();
		execStmts();
	}
	
}

void Compiler::execStmt(){
	if(!isNonKeyId(token) && token != "read" && token != "write"
      && token != "if" && token != "while" && token != "repeat"
      && token != ";" && token != "begin" && token != "until"){
		processError("expected one of \"read\", \"write\", \"if\", \"while\", \"repeat\", \";\", \"begin\", \"until\", or non-keyword identifier");
	}
	
	if(isNonKeyId(token)){
		assignStmt();
	}else if(token == "read"){
		readStmt();
	}else if(token == "write"){
		writeStmt();
	}else if(token == "if"){
      ifStmt();
   }else if(token == "while"){
      whileStmt();
   }else if(token == "repeat"){
      repeatStmt();
   }else if(token == ";"){
      nullStmt();
   }else if(token == "begin"){
      beginEndStmt();
   }
	
}

void Compiler::assignStmt(){
   if(!isNonKeyId(token))
      processError("non-keyword identifier expected");
   pushOperand(token);
   if(nextToken() != ":=")
      processError("\":=\" expected");
   pushOperator(token);
   if(nextToken() != "not" && token != "true" && token != "false" && token != "("
	  && token != "+" && token != "-" && !isInteger(token) && !isNonKeyId(token))
	  processError("expected one of 'not', 'true', 'false', '(', '+', '-', an integer, or a non-keyword identifier");
	express();
   if(token != ";")
      processError("';' expected");
   code(popOperator(), popOperand(), popOperand());
   nextToken();
}

void Compiler::readStmt(){
   if(token != "read")
      processError("Keyword \"read\" expected");
   if(nextToken() != "(")
      processError("'(' expected");
   
   string x = "";
   
   nextToken();
   while(token != ")"){
      if(!isNonKeyId(token)){
         if(x.back() == ',')
            processError("non-keyword identifier expected");
         else
            processError("')' expected");
      }
      x += token;
      if(nextToken() == ","){
         x += ",";
         nextToken();
         if(token == ")")
            processError("non-keyword identifier expected");
      }
   }
   
   code("read", x);
   
   if(token != ")")
      processError("')' expected");
   if(nextToken() != ";")
      processError("';' expected");
   nextToken();
   
}

void Compiler::writeStmt(){
   if(token != "write")
      processError("compiler error: write expected");
   nextToken();
   if(token != "(")
      processError("'(' expected");
   nextToken();
   
   string x = "";
   while(token != ")"){
      if(!isNonKeyId(token))
         processError("non-keyword identifier expected");
      x += token;
      if(nextToken() == ","){
         x += ",";
         nextToken();
         if(token == ")")
            processError("non-keyword identifier expected");
      }else if(token != "," && token != ")"){
         processError("')' or ',' expected");
      }
   }
   nextToken();
   code("write", x);
   if(token != ";")
      processError("';' expected");
   nextToken();
   
   
}

void Compiler::express(){
   
   // EXPRESS -> >TERM< EXPRESSES
   if(token != "not" && token != "true" && token != "false" && token != "("
	  && token != "+" && token != "-" && !isInteger(token) && !isNonKeyId(token))
     processError("expression expected");
   term();
   
   // EXPRESS -> TERM >EXPRESSES<
   if(token != "<>" && token != "=" && token != "<=" &&
      token != ">=" && token != "<" && token != ">" &&
      token != ")" && token != ";" && token != "then"
      && token != "do")
      processError("one of \"*\", \"and\", \"div\", \"mod\", \")\", \"+\", \"-\", \";\", \"<\", \"<=\", \"<>\", \"=\", \">\", \">=\", \"or\", \"then\", or \"do\" expected");
   expresses();
   
}
void Compiler::expresses(){
   if(token == "=" || token == "<>" || token == "<="
      || token == ">=" || token == "<" || token == ">"){
      
      pushOperator(token);
      nextToken();
      term();
      code(popOperator(), popOperand(), popOperand());
      expresses();
      
   }
}

void Compiler::term(){
   if(token != "not" && token != "true" && token != "false" && token != "("
	  && token != "+" && token != "-" && !isInteger(token) && !isNonKeyId(token))
   processError("expression expected");
   
   // TERM -> >FACTOR< TERMS
   // Already did error processing above.
   factor();
   terms();
}
void Compiler::terms(){
   if(token == "-" || token == "+" || token == "or"){
      pushOperator(token);
      nextToken();
      factor();
      code(popOperator(), popOperand(), popOperand());
      terms();
   }
}

void Compiler::factor(){
   if(token != "not" && token != "true" && token != "false" && token != "("
	  && token != "+" && token != "-" && !isInteger(token) && !isNonKeyId(token))
     processError("expression expected");
   part();
   factors();
}
void Compiler::factors(){
   if(token == "*" || token == "div" || token == "mod" || token == "and"){
      pushOperator(token);
      nextToken();
      part();
      code(popOperator(),popOperand(),popOperand());
      factors();
   }
}

void Compiler::part(){
   // PART -> 'not'
   if(token == "not"){
      nextToken();
      // PART -> 'not' '(' EXPRESS ')'
      if(token == "("){
         nextToken();
         express();
         if(token != ")"){
            processError("')' expected");
         }
         nextToken();
         code("not", popOperand());
      // PART -> 'not' BOOLEAN
      }else if(isBoolean(token)){
         pushOperand((token == "true" ? "false": "true"));
         nextToken();
      }else if(isNonKeyId(token)){
         code("not", token);
         nextToken();
      }else{
         processError("expected '(', boolean, or non-keyword id");
      }
   }else if(token == "+"){
      nextToken();
      if(token == "("){
         nextToken();
         express();
         if(token != ")"){
            processError("')' expected");
         }
         nextToken();
      }else if(isInteger(token) || isNonKeyId(token)){
         pushOperand(token);
         nextToken();
      }else{
         processError("expected '(', integer, or non-keyword id");
      }
   }else if(token == "-"){
      nextToken();
      if(token == "("){
         nextToken();
         express();
         if(token != ")"){
            processError("')' expected");
         }
         nextToken();
         code("neg",popOperand());
      }else if(isInteger(token)){
         pushOperand("-"+token);
         nextToken();
      }else if(isNonKeyId(token)){
         code("neg", token);
         nextToken();
      }else{
         processError("expected '(', integer, or non-keyword id");
      }
      
   }else if(isInteger(token) || isBoolean(token) || isNonKeyId(token)){
      pushOperand(token);
      nextToken();
   }else if(token == "("){
      nextToken();
      express();
      if(token != ")"){
         processError("')' expected");
      }
      nextToken();
   }else{
      processError("invalid token in expression");
   }
}

// Stage 2 Additions
void Compiler::ifStmt(){
   if(token != "if")
      processError("compiler error: if not received in ifStmt()");
   nextToken();
   express();
   if(token != "then")
      processError("'then' expected");
   code("then", popOperand());
   nextToken();
   execStmt();
   elsePt();
}
void Compiler::elsePt(){
   if(token == "else"){
      code("else", popOperand());
      nextToken();
      execStmt();
   }
   code("post_if", popOperand());
}
void Compiler::whileStmt(){
   
   // WHILE -> 'while'
   if(token != "while")
      processError("compiler error: while expected but not received");
   code("while");
   nextToken();
   // WHILE -> 'while' EXPRESS
   express();
   // WHILE -> 'while' EXPRESS 'do'
   if(token != "do")
      processError("'do' expected");
   code("do", popOperand());
   nextToken();
   // WHILE -> 'while' EXPRESS 'do' EXEC_STMT
   execStmt();
   
   // end while
   code("post_while", popOperand(), popOperand());
};
void Compiler::repeatStmt(){
   if(token != "repeat")
      processError("compiler error: repeatStmt expects 'repeat'");
   code("repeat");
   nextToken();
   execStmts();
   if(token != "until")
      processError("'until' expected");
   nextToken();
   express();
   code("until",popOperand(),popOperand());
   if(token != ";")
      processError("';' expected");
   nextToken();
};
void Compiler::nullStmt(){
   if(token != ";")
      processError("compiler error: nullStmt expects ';'");
   nextToken();
};

/* -----------
HELPER FUNCTIONS
*/

bool Compiler::isKeyword(string s) const {
   const char* keywords[23] = {"program", "const", "var", "integer", "boolean",
                              "begin", "end", "true", "false", "not", "mod", "div",
                              "and", "or", "read", "write", "if", "then", "else", "while",
                              "do", "repeat", "until"};
   
   for(uint i = 0; i < 23; i++){
      
      if(s == keywords[i])
         return true;
      
   }
   
   return false;
   
}

bool Compiler::isSpecialSymbol(char c) const{
   return !isalnum(c);
}

bool Compiler::isNonKeyId(string s) const{
   
   auto itr = s.begin();
   
   if(isKeyword(s))
      return false;
   
   if(!isalpha(*itr))
      return false;
   
   for(itr++; itr < s.end(); itr++){
      
      if(!isalpha(*itr) && !isdigit(*itr) && *itr != '_')
         return false;
      
   }
   
   return true;
   
}

bool Compiler::isInteger(string s) const{
   string::iterator it = s.begin();
   while (it != s.end() && isdigit(*it)) ++it;
   return !s.empty() && it==s.end();
}

bool Compiler::isBoolean(string s) const{
   return s == "true" || s == "false";
}

bool Compiler::isLiteral(string s) const{
   
   if(isInteger(s) || isBoolean(s)) return true;
   
   if(s[0] == '+' || s[0] == '-')
      if(isInteger(s.substr(1, string::npos))) return true;
   
   
   if(s.substr(0, 3) == "not")
      if(isBoolean(s.substr(3, string::npos))) return true;
   
   return false;
}

/* -----------
ACTION ROUTINES
*/

void Compiler::insert(string externalName, storeTypes inType, modes inMode,
            string inValue, allocation inAlloc, int inUnits){
   static int tableSize = 0;
   string name;
   
   name = externalName.substr(0, externalName.find(','));
   externalName.erase(0, (externalName.find(',') == string::npos) ? string::npos : externalName.find(',')+1);
   
   while(name != ""){
      if(symbolTable.find(name) != symbolTable.end())
         processError("symbol x is multiply defined");
      else if(name.length() > 15)
         processError("non keyword identifiers must be at most 15 characters in length");
      else if (isKeyword(name))
         processError("illegal use of keyword");
      else{
         tableSize++;
         if(tableSize > 256)
            processError("symbol table size exceeds 256 entries");
         if(isupper(name[0]))
            symbolTable.insert(pair<string, SymbolTableEntry>(name,
                               SymbolTableEntry(name,inType,inMode,inValue,inAlloc,inUnits)));
         else
            symbolTable.insert(pair<string, SymbolTableEntry>(name,
                               SymbolTableEntry(genInternalName(inType),inType,inMode,inValue,inAlloc,inUnits)));
      }
      
      name = externalName.substr(0, externalName.find(','));
      externalName.erase(0, externalName.find(','));
      externalName.erase(0,1);
      
   }
}

storeTypes Compiler::whichType(string name){
   
   storeTypes dtype;
   if(isLiteral(name)){
      if(isBoolean(name)){
         dtype = BOOLEAN;
         
      }else{
         dtype = INTEGER;
      }
      
   }else{
      if(symbolTable.find(name) != symbolTable.end())
         dtype = symbolTable.at(name).getDataType();
      else{
         processError("reference to undefined symbol " + name);
      }
   }
   return dtype;
   
}

string Compiler::whichValue(string name){
   
   string value;
   
   if(isLiteral(name)){
      value = name;
   }else{
      if((symbolTable.find(name) != symbolTable.end()) && (symbolTable.at(name).getValue() != ""))
         value = symbolTable.at(name).getValue();
      else
         processError("reference to undefined constant:" + name + " : " + to_string((int)(name[0])));
   }
   return value;
}

void Compiler::code(string op, string operand1, string operand2){
   if(op == "program")
      emitPrologue(operand1);
   else if (op == "end"){
      if(operand1 == ".")
         emitEpilogue();
   }
   else if (op == "read")
      emitReadCode(operand1);
   else if (op == "write")
      emitWriteCode(operand1);
   else if (op == "neg")
      emitNegationCode(operand1);
   else if (op == "not")
      emitNotCode(operand1);
   else if (op == "div")
      emitDivisionCode(operand1, operand2);
   else if (op == "mod")
      emitModuloCode(operand1, operand2);
   else if (op == "and")
      emitAndCode(operand1, operand2);
   else if (op == "+")
      emitAdditionCode(operand1, operand2);
   else if (op == "-")
      emitSubtractionCode(operand1, operand2);
   else if (op == "or")
      emitOrCode(operand1, operand2);
   else if (op == "*")
      emitMultiplicationCode(operand1, operand2);
   else if (op == ":=")
      emitAssignCode(operand2, operand1);
   else if (op == "=")
      emitEqualityCode(operand1, operand2);
   else if (op == "<>")
      emitInequalityCode(operand1, operand2);
   else if (op == "<=")
      emitLessThanOrEqualToCode(operand1, operand2);
   else if (op == ">=")
      emitGreaterThanOrEqualToCode(operand1, operand2);
   else if (op == "<")
      emitLessThanCode(operand1, operand2);
   else if (op == ">")
      emitGreaterThanCode(operand1, operand2);
   else if (op == "then")
      emitThenCode(operand1, operand2);
   else if (op == "else")
      emitElseCode(operand1, operand2);
   else if (op == "post_if")
      emitPostIfCode(operand1, operand2);
   else if (op == "while")
      emitWhileCode();
   else if (op == "do")
      emitDoCode(operand1);
   else if (op == "post_while")
      emitPostWhileCode(operand1, operand2);
   else if (op == "repeat")
      emitRepeatCode();
   else if (op == "until")
      emitUntilCode(operand1, operand2);
   else
      processError("compiler error: illegal arguments to Compiler::code");
}

// Stage 1 Additions

void Compiler::pushOperator(string op){
   operatorStk.push(op);
}

string Compiler::popOperator(){
   string x;
   if(!operatorStk.empty()){
      x = operatorStk.top();
      operatorStk.pop();
      return x;
   }else
      processError("compiler error; operator stack underflow");
   return ""; // suppress warning, will never reach this
}

void Compiler::pushOperand(string operand){
   if(isLiteral(operand) && symbolTable.find(operand) == symbolTable.end()){
      if(operand == "true" || operand == "false"){
         // insert manually
         
         symbolTable.insert(pair<string, SymbolTableEntry>(operand,
                               SymbolTableEntry(operand == "true" ? "TRUE" : "FALSE",BOOLEAN,CONSTANT,operand,YES,1)));
      }else{
         for(auto itr = symbolTable.begin(); itr != symbolTable.end(); itr++){
            if(itr->second.getValue() == operand && itr->second.getMode() == CONSTANT){
               operand = itr->first;
               operandStk.push(operand);
               return;
            }
         }
         insert(operand, whichType(operand), CONSTANT, whichValue(operand), YES, 1);
      }
   }
   operandStk.push(operand);
}

string Compiler::popOperand(){
   string x;
   if(!operandStk.empty()){
      x = operandStk.top();
      operandStk.pop();
      return x;
   }else
      processError("compiler error; operand stack underflow. token: " + token);
   return ""; // suppress warning, will never reach this
}


/* -----------
EMIT FUNCTIONS
*/

void Compiler::emit(string label, string instruction, string operands,
            string comment){
      
      objectFile << left;
      objectFile << setw(8) << label;
      objectFile << setw(8) << instruction;
      objectFile << setw(24) << operands;
      objectFile << comment;
      objectFile << endl;
      
   }

void Compiler::emitPrologue(string progName, string s){
     time_t now = time (NULL);
     
      objectFile << left;
      objectFile << "; " << setw(19) << "Hayden Lester" << ctime(&now);
      objectFile << "%INCLUDE \"Along32.inc\"\n";
      objectFile << "%INCLUDE \"Macros_Along.inc\"\n\n";
      emit("SECTION", ".text");
      emit("global", "_start", "", "; program " + progName);
      objectFile << endl;
      emit("_start:");
  }

void Compiler::emitEpilogue(string s, string e){
     emit("","Exit","{0}");
     emitStorage();
  }
  
void Compiler::emitStorage(){
   objectFile << '\n';
   emit("SECTION", ".data");
   for(auto itr = symbolTable.begin(); itr != symbolTable.end(); itr++){
      if(itr->second.getAlloc() == YES && itr->second.getMode() == CONSTANT){
         emit(itr->second.getInternalName(), "dd",
             ((itr->second.getDataType() == BOOLEAN) ? ((itr->second.getValue() == "false") ? "0" : "-1") : itr->second.getValue()),
             "; " + itr->first);
      }
   }
   objectFile << '\n';
   emit("SECTION", ".bss");
   for(auto itr = symbolTable.begin(); itr != symbolTable.end(); itr++){
      if(itr->second.getAlloc() == YES && itr->second.getMode() == VARIABLE){
         emit(itr->second.getInternalName(), "resd", itr->second.getValue() == "" ? "1" : itr->second.getValue(), "; " + itr->first);
      }
   }
}

// Stage 1 Additions


void Compiler::emitReadCode(string operand, string operand2){
   
   string name;
   while((name = operand.substr(0, operand.find(','))) != ""){
      // set operand to everything past the first ","
      operand = operand.substr(operand.find(',') == string::npos ? operand.length() : operand.find(',')+1, string::npos);
      
      // error checking
      if(symbolTable.find(name) == symbolTable.end())
         processError("reference to undefined variable '" + name + "'");
      if(symbolTable.at(name).getDataType() != INTEGER)
         processError("can't read variables of this type");
      if(symbolTable.at(name).getMode() != VARIABLE)
         processError("attempting to read to a read-only location: " + name);
        
      // emits
      emit("", "call", "ReadInt", "; read int; value placed in eax");
      emit("", "mov", "["+symbolTable.at(name).getInternalName()+"],eax", "; store eax at " + name);
      contentsOfAReg = name;
      
   }
   
}

void Compiler::emitWriteCode(string operand, string operand2){
   
   string name;
   while((name = operand.substr(0, operand.find(','))) != ""){
      // set operand to everything past the first ","
      operand = operand.substr(operand.find(',') == string::npos ? operand.length() : operand.find(',')+1, string::npos);
      
      // error checking
      if(symbolTable.find(name) == symbolTable.end())
         processError("reference to undefined symbol '" + name + "'");
      
      // load name into eax
      if(contentsOfAReg != name){
         emit("","mov","eax,["+symbolTable.at(name).getInternalName()+"]", "; load " + name + " in eax");
         contentsOfAReg = name;
      }
      if(whichType(name) == INTEGER || whichType(name) == BOOLEAN){
         emit("","call","WriteInt","; write int in eax to standard out");
      }
      emit("", "call", "Crlf", "; write \\r\\n to standard out");
      
   }
   
}

void Compiler::emitAssignCode(string operand1, string operand2){
   if(whichType(operand1) != whichType(operand2))
      processError("incompatible types for operator ':='");
   if(symbolTable.at(operand2).getMode() != VARIABLE)
      processError("symbol on left-hand side of assignment must have a storage mode of VARIABLE");
   if(operand1 == operand2) return;
   if(operand1 != contentsOfAReg){
      emit("", "mov", "eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
   }
   emit("", "mov", "["+symbolTable.at(operand2).getInternalName()+"],eax", "; " + operand2 + " = AReg");
   if(isTemporary(operand1)){
      symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
   }
   contentsOfAReg = operand2;
}

void Compiler::emitAdditionCode(string operand1, string operand2){
   // check for illegal types
   if(whichType(operand1) != INTEGER || whichType(operand2) != INTEGER)
      processError("binary '+' requires integer operands");
   
   // evacuate temps
   if(isTemporary(contentsOfAReg) && contentsOfAReg != operand2 && contentsOfAReg != operand1){
      emit("", "mov", "["+contentsOfAReg+"],eax","; deassign AReg");
      symbolTable.at(contentsOfAReg).setAlloc(YES);
      symbolTable.at(contentsOfAReg).setValue("");
      contentsOfAReg = "";
   }
   // deassign
   if(!isTemporary(contentsOfAReg) && contentsOfAReg != operand2 && contentsOfAReg != operand1){
      // deassign
      contentsOfAReg = "";
   }
   // load operand if needed
   if(contentsOfAReg != operand1 && contentsOfAReg != operand2){
      // load operand 2 into register, arbitrary choice
      emit("","mov","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
      contentsOfAReg = operand1;
   }
   
   if(contentsOfAReg == operand1){
      emit("","add","eax,["+symbolTable.at(operand2).getInternalName()+"]", "; AReg = " + operand1 + " + " + operand2);
   }else{
      emit("","add","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand2 + " + " + operand1);
   }
   // deassign all temporaries
   if(isTemporary(operand1)){
		symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
	}
   if(isTemporary(operand2)){
		symbolTable.at(operand2).setValue("");
		symbolTable.at(operand2).setDataType(UNKNOWN);
		freeTemp();
	}
   
   string t = getTemp();
	symbolTable.at(t).setDataType(INTEGER);
	contentsOfAReg = t;
	pushOperand(t);
   
}

void Compiler::emitSubtractionCode(string operand1, string operand2){
   // check for illegal types
   if(whichType(operand1) != INTEGER || whichType(operand2) != INTEGER)
      processError("binary '-' expects integer operands");
   
   // evacuate temps
   if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1){
      emit("", "mov", "["+contentsOfAReg+"],eax","; deassign AReg");
      symbolTable.at(contentsOfAReg).setAlloc(YES);
      symbolTable.at(contentsOfAReg).setValue("");
      contentsOfAReg = "";
   }
   // deassign
   if(!isTemporary(contentsOfAReg) && contentsOfAReg != operand1){
      // deassign
      contentsOfAReg = "";
   }
   // load operand if needed
   if(contentsOfAReg != operand1){
      // load operand 2 into register, arbitrary choice
      emit("","mov","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
      contentsOfAReg = operand1;
   }
   
   emit("","sub","eax,["+symbolTable.at(operand2).getInternalName()+"]", "; AReg = " + operand1 + " - " + operand2);
   
   // deassign all temporaries
   if(isTemporary(operand1)){
		symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
	}
   if(isTemporary(operand2)){
		symbolTable.at(operand2).setValue("");
		symbolTable.at(operand2).setDataType(UNKNOWN);
		freeTemp();
	}
   
   string t = getTemp();
	symbolTable.at(t).setDataType(INTEGER);
	contentsOfAReg = t;
	pushOperand(t);
   
}

void Compiler::emitMultiplicationCode(string operand1, string operand2){
   // check for illegal types
   if(whichType(operand1) != INTEGER || whichType(operand2) != INTEGER)
      processError("binary '*' expects integer operands");
   
   // evacuate temps
   if(isTemporary(contentsOfAReg) && contentsOfAReg != operand2 && contentsOfAReg != operand1){
      emit("", "mov", "["+contentsOfAReg+"],eax","; deassign AReg");
      symbolTable.at(contentsOfAReg).setAlloc(YES);
      symbolTable.at(contentsOfAReg).setValue("");
      contentsOfAReg = "";
   }
   // deassign
   if(!isTemporary(contentsOfAReg) && contentsOfAReg != operand2 && contentsOfAReg != operand1){
      // deassign
      contentsOfAReg = "";
   }
   // load operand if needed
   if(contentsOfAReg != operand1 && contentsOfAReg != operand2){
      // load operand 2 into register, arbitrary choice
      emit("","mov","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
      contentsOfAReg = operand1;
   }
   
   if(contentsOfAReg == operand2){
      emit("","imul","dword ["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand2 + " * " + operand1);
   }else{
      emit("","imul","dword ["+symbolTable.at(operand2).getInternalName()+"]", "; AReg = " + operand1 + " * " + operand2);
   }
   // deassign all temporaries
   if(isTemporary(operand1)){
		symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
	}
   if(isTemporary(operand2)){
		symbolTable.at(operand2).setValue("");
		symbolTable.at(operand2).setDataType(UNKNOWN);
		freeTemp();
	}
   
   string t = getTemp();
	symbolTable.at(t).setDataType(INTEGER);
	contentsOfAReg = t;
	pushOperand(t);
   
}

void Compiler::emitDivisionCode(string operand1, string operand2){
   // check for illegal types
   if(whichType(operand1) != INTEGER || whichType(operand2) != INTEGER)
      processError("binary 'div' expects integer operands");
   
   // evacuate temps
   if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1){
      emit("", "mov", "["+contentsOfAReg+"],eax","; deassign AReg");
      symbolTable.at(contentsOfAReg).setAlloc(YES);
      symbolTable.at(contentsOfAReg).setValue("");
      contentsOfAReg = "";
   }
   // deassign
   if(!isTemporary(contentsOfAReg) && contentsOfAReg != operand1){
      // deassign
      contentsOfAReg = "";
   }
   // load operand if needed
   if(contentsOfAReg != operand1){
      // load operand 2 into register, arbitrary choice
      emit("","mov","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
      contentsOfAReg = operand1;
   }
   
   emit("","cdq","","; sign extend dividend from eax to edx:eax");
   
   emit("","idiv","dword ["+symbolTable.at(operand2).getInternalName()+"]", "; AReg = " + operand1 + " div " + operand2);
   
   // deassign all temporaries
   if(isTemporary(operand1)){
		symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
	}
   if(isTemporary(operand2)){
		symbolTable.at(operand2).setValue("");
		symbolTable.at(operand2).setDataType(UNKNOWN);
		freeTemp();
	}
   
   string t = getTemp();
	symbolTable.at(t).setDataType(INTEGER);
	contentsOfAReg = t;
	pushOperand(t);
   
}

void Compiler::emitModuloCode(string operand1, string operand2){
   // check for illegal types
   if(whichType(operand1) != INTEGER || whichType(operand2) != INTEGER)
      processError("binary 'mod' expects integer operands");
   
   // evacuate temps
   if(isTemporary(contentsOfAReg) && contentsOfAReg != operand2){
      emit("", "mov", "["+contentsOfAReg+"],eax","; deassign AReg");
      symbolTable.at(contentsOfAReg).setAlloc(YES);
      symbolTable.at(contentsOfAReg).setValue("");
      contentsOfAReg = "";
   }
   // deassign
   if(!isTemporary(contentsOfAReg) && contentsOfAReg != operand2){
      // deassign
      contentsOfAReg = "";
   }
   // load operand if needed
   if(contentsOfAReg != operand1){
      // load operand 2 into register, arbitrary choice
      emit("","mov","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
      contentsOfAReg = operand1;
   }
   
   emit("","cdq","","; sign extend dividend from eax to edx:eax");
   
   emit("","idiv","dword ["+symbolTable.at(operand2).getInternalName()+"]", "; AReg = " + operand1 + " div " + operand2);
   
   emit("","xchg","eax,edx","; exchange quotient and remainder");
   
   // deassign all temporaries
   if(isTemporary(operand1)){
		symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
	}
   if(isTemporary(operand2)){
		symbolTable.at(operand2).setValue("");
		symbolTable.at(operand2).setDataType(UNKNOWN);
		freeTemp();
	}
   
   string t = getTemp();
	symbolTable.at(t).setDataType(INTEGER);
	contentsOfAReg = t;
	pushOperand(t);
   
}

void Compiler::emitNegationCode(string operand1, string operand2){
   // check for illegal types
   if(whichType(operand1) != INTEGER)
      processError("unary '-' expects integer operand");
   
   // evacuate any temps
   if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1){
      emit("", "mov", "["+contentsOfAReg+"],eax","; deassign eax");
      symbolTable.at(contentsOfAReg).setAlloc(YES);
      symbolTable.at(contentsOfAReg).setValue("");
   }
   
   if(!isTemporary(contentsOfAReg) && contentsOfAReg != operand2){
      // deassign
      contentsOfAReg = "";
   }
   
   if(contentsOfAReg != operand1){
		emit("","mov","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
	}
   
   // perform the not
	emit("", "neg", "eax", "; AReg = -AReg");
	// deassign involved temporaries
	if(isTemporary(operand1)){
		symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
	}
	string t = getTemp();
	symbolTable.at(t).setDataType(INTEGER);
	contentsOfAReg = t;
	pushOperand(t);
   
}

void Compiler::emitNotCode(string operand1, string operand2){
	
	//check for illegal types
	if(whichType(operand1) != BOOLEAN)
		processError("unary 'not' expects a boolean operand");
	
	// evacuate any temps
	if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1){
		emit("","mov","["+contentsOfAReg+"],eax","; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		symbolTable.at(contentsOfAReg).setValue(""); // is this deassigning? I DONT KNOW
      contentsOfAReg = "";
	}
   if(!isTemporary(contentsOfAReg) && contentsOfAReg != operand1){
		contentsOfAReg = "";
	}
	
	// load operand1 into memory
	if(contentsOfAReg != operand1){
		emit("","mov","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
      contentsOfAReg = operand1;
	}
	// perform the not
	emit("", "not", "eax", "; AReg = !AReg");
	// deassign involved temporaries
	if(isTemporary(operand1)){
		symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
	}
	string t = getTemp();
	symbolTable.at(t).setDataType(BOOLEAN);
	contentsOfAReg = t;
	pushOperand(t);
}

void Compiler::emitAndCode(string operand1, string operand2){
	
	//check for illegal types
	if(whichType(operand1) != BOOLEAN && whichType(operand2) != BOOLEAN)
		processError("binary 'and' expects boolean operands");
	
	// evacuate any temps
	if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2){
		emit("","mov","["+contentsOfAReg+"],eax","; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		symbolTable.at(contentsOfAReg).setValue(""); // is this deassigning? I DONT KNOW
      contentsOfAReg = "";
	}
   if(!isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2){
		contentsOfAReg = "";
	}
	
	// load operand1 into memory
	if(contentsOfAReg != operand1 && contentsOfAReg != operand2){
		emit("","mov","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
      contentsOfAReg = operand1;
	}
	// perform the and
   
   if(contentsOfAReg == operand1){
      emit("","and","eax,["+symbolTable.at(operand2).getInternalName()+"]", "; AReg = " + contentsOfAReg + " and " + operand2);
   }else{
      emit("","and","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + contentsOfAReg + " and " + operand1);
   }
   
	// deassign involved temporaries
	if(isTemporary(operand1)){
		symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
	}
   if(isTemporary(operand2)){
		symbolTable.at(operand2).setValue("");
		symbolTable.at(operand2).setDataType(UNKNOWN);
		freeTemp();
	}
	string t = getTemp();
	symbolTable.at(t).setDataType(BOOLEAN);
	contentsOfAReg = t;
	pushOperand(t);
}

void Compiler::emitOrCode(string operand1, string operand2){
	
	//check for illegal types
	if(whichType(operand1) != BOOLEAN || whichType(operand2) != BOOLEAN)
		processError("binary 'or' expects boolean operands");
	
	// evacuate any temps
	if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2){
		emit("","mov","["+contentsOfAReg+"],eax","; deassign AReg");
		symbolTable.at(contentsOfAReg).setAlloc(YES);
		symbolTable.at(contentsOfAReg).setValue(""); // is this deassigning? I DONT KNOW
      contentsOfAReg = "";
	}
   if(!isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2){
		contentsOfAReg = "";
	}
	
	// load operand1 into memory
	if(contentsOfAReg != operand1 && contentsOfAReg != operand2){
		emit("","mov","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
      contentsOfAReg = operand1;
	}
	// perform the or
   
   if(contentsOfAReg == operand1){
      emit("","or","eax,["+symbolTable.at(operand2).getInternalName()+"]", "; AReg = " + contentsOfAReg + " or " + operand2);
   }else{
      emit("","or","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + contentsOfAReg + " or " + operand1);
   }
   
	// deassign involved temporaries
	if(isTemporary(operand1)){
		symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
	}
   if(isTemporary(operand2)){
		symbolTable.at(operand2).setValue("");
		symbolTable.at(operand2).setDataType(UNKNOWN);
		freeTemp();
	}
	string t = getTemp();
	symbolTable.at(t).setDataType(BOOLEAN);
	contentsOfAReg = t;
	pushOperand(t);
}

void Compiler::emitEqualityCode(string operand1, string operand2){
   // check for illegal types
   if(whichType(operand1) != whichType(operand2))
      processError("operator '=' requires that operands must be of the same type");
   
   // evacuate temps
   if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2){
      emit("", "mov", "["+contentsOfAReg+"],eax","; deassign AReg");
      symbolTable.at(contentsOfAReg).setAlloc(YES);
      symbolTable.at(contentsOfAReg).setValue("");
      contentsOfAReg = "";
   }
   
   if(!isTemporary(contentsOfAReg) && contentsOfAReg != operand2 && contentsOfAReg != operand1){
      // deassign
      contentsOfAReg = "";
   }
   // load operand if needed
   if(contentsOfAReg != operand2 && contentsOfAReg != operand1){
      // load operand 1 into register, arbitrary choice
      emit("","mov","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
      contentsOfAReg = operand1;
   }
   
   // perform comparison
   string x = getLabel();
   if(contentsOfAReg == operand1){
      emit("","cmp","eax,["+symbolTable.at(operand2).getInternalName()+"]","; compare " + contentsOfAReg + " and " + operand2);
      emit("", "je", x, "; if " + operand2 + " = " + contentsOfAReg + " then jump to set eax to TRUE");
   }else{
      emit("","cmp","eax,["+symbolTable.at(operand1).getInternalName()+"]","; compare " + contentsOfAReg + " and " + operand1);
      emit("", "je", x, "; if " + operand1 + " = " + contentsOfAReg + " then jump to set eax to TRUE");
   }
   emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");
   // if false doesn't exist, insert it
   if(symbolTable.find("false") == symbolTable.end()){
      symbolTable.insert(pair<string, SymbolTableEntry>("false",
                               SymbolTableEntry("FALSE",BOOLEAN,CONSTANT,"false",YES,1)));
   }
   string y = getLabel();
   emit("", "jmp", y, "; unconditionally jump");
   emit(x+":","","","");
   emit("","mov", "eax,[TRUE]", "; set eax to TRUE");
   // if true doesn't exist, insert it
   if(symbolTable.find("true") == symbolTable.end()){
      symbolTable.insert(pair<string, SymbolTableEntry>("true",
                               SymbolTableEntry("TRUE",BOOLEAN,CONSTANT,"true",YES,1)));
   }
   emit(y+":","","","");
   
   // deassign all temporaries
   if(isTemporary(operand1)){
		symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
	}
   if(isTemporary(operand2)){
		symbolTable.at(operand2).setValue("");
		symbolTable.at(operand2).setDataType(UNKNOWN);
		freeTemp();
	}
   
   string t = getTemp();
	symbolTable.at(t).setDataType(BOOLEAN);
	contentsOfAReg = t;
	pushOperand(t);
   
}

void Compiler::emitInequalityCode(string operand1, string operand2){
   // check for illegal types
   if(whichType(operand1) != whichType(operand2))
      processError("operator '<>' requires that operands must be of the same type");
   
   // evacuate temps
   if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1 && contentsOfAReg != operand2){
      emit("", "mov", "["+contentsOfAReg+"],eax","; deassign AReg");
      symbolTable.at(contentsOfAReg).setAlloc(YES);
      symbolTable.at(contentsOfAReg).setValue("");
      contentsOfAReg = "";
   }
   
   if(!isTemporary(contentsOfAReg) && contentsOfAReg != operand2 && contentsOfAReg != operand1){
      // deassign
      contentsOfAReg = "";
   }
   // load operand if needed
   if(contentsOfAReg != operand2 && contentsOfAReg != operand1){
      // load operand 1 into register, arbitrary choice
      emit("","mov","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
      contentsOfAReg = operand1;
   }
   
   // perform comparison
   string x = getLabel();
   if(contentsOfAReg == operand1){
      emit("","cmp","eax,["+symbolTable.at(operand2).getInternalName()+"]","; compare " + contentsOfAReg + " and " + operand2);
      emit("", "jne", x, "; if " + operand2 + " <> " + contentsOfAReg + " then jump to set eax to TRUE");
   }else{
      emit("","cmp","eax,["+symbolTable.at(operand1).getInternalName()+"]","; compare " + contentsOfAReg + " and " + operand1);
      emit("", "jne", x, "; if " + operand1 + " <> " + contentsOfAReg + " then jump to set eax to TRUE");
   }
   emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");
   // if false doesn't exist, insert it
   if(symbolTable.find("false") == symbolTable.end()){
      symbolTable.insert(pair<string, SymbolTableEntry>("false",
                               SymbolTableEntry("FALSE",BOOLEAN,CONSTANT,"false",YES,1)));
   }
   string y = getLabel();
   emit("", "jmp", y, "; unconditionally jump");
   emit(x+":","","","");
   emit("","mov", "eax,[TRUE]", "; set eax to TRUE");
   // if true doesn't exist, insert it
   if(symbolTable.find("true") == symbolTable.end()){
      symbolTable.insert(pair<string, SymbolTableEntry>("true",
                               SymbolTableEntry("TRUE",BOOLEAN,CONSTANT,"true",YES,1)));
   }
   emit(y+":","","","");
   
   // deassign all temporaries
   if(isTemporary(operand1)){
		symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
	}
   if(isTemporary(operand2)){
		symbolTable.at(operand2).setValue("");
		symbolTable.at(operand2).setDataType(UNKNOWN);
		freeTemp();
	}
   
   string t = getTemp();
	symbolTable.at(t).setDataType(BOOLEAN);
	contentsOfAReg = t;
	pushOperand(t);
   
}

void Compiler::emitLessThanCode(string operand1, string operand2){
   // check for illegal types
   if(whichType(operand1) != INTEGER || whichType(operand2) != INTEGER)
      processError("binary '<' requires integer operands");
   
   // evacuate temps
   if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1){
      emit("", "mov", "["+contentsOfAReg+"],eax","; deassign AReg");
      symbolTable.at(contentsOfAReg).setAlloc(YES);
      symbolTable.at(contentsOfAReg).setValue("");
      contentsOfAReg = "";
   }
   
   if(!isTemporary(contentsOfAReg) && contentsOfAReg != operand1){
      // deassign
      contentsOfAReg = "";
   }
   // load operand if needed
   if(contentsOfAReg != operand1){
      // load operand 2 into register, arbitrary choice
      emit("","mov","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
      contentsOfAReg = operand1;
   }
   
   // perform comparison
   emit("","cmp","eax,["+symbolTable.at(operand2).getInternalName()+"]","; compare " + contentsOfAReg + " and " + operand2);
   string x = getLabel();
   emit("", "jl", x, "; if " + contentsOfAReg + " < " + operand2 + " then jump to set eax to TRUE");
   emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");
   // if false doesn't exist, insert it
   if(symbolTable.find("false") == symbolTable.end()){
      symbolTable.insert(pair<string, SymbolTableEntry>("false",
                               SymbolTableEntry("FALSE",BOOLEAN,CONSTANT,"false",YES,1)));
   }
   string y = getLabel();
   emit("", "jmp", y, "; unconditionally jump");
   emit(x+":","","","");
   emit("","mov", "eax,[TRUE]", "; set eax to TRUE");
   // if true doesn't exist, insert it
   if(symbolTable.find("true") == symbolTable.end()){
      symbolTable.insert(pair<string, SymbolTableEntry>("true",
                               SymbolTableEntry("TRUE",BOOLEAN,CONSTANT,"true",YES,1)));
   }
   emit(y+":","","","");
   
   // deassign all temporaries
   if(isTemporary(operand1)){
		symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
	}
   if(isTemporary(operand2)){
		symbolTable.at(operand2).setValue("");
		symbolTable.at(operand2).setDataType(UNKNOWN);
		freeTemp();
	}
   
   string t = getTemp();
	symbolTable.at(t).setDataType(BOOLEAN);
	contentsOfAReg = t;
	pushOperand(t);
   
}

void Compiler::emitLessThanOrEqualToCode(string operand1, string operand2){
   // check for illegal types
   if(whichType(operand1) != INTEGER || INTEGER != whichType(operand2))
      processError("operator '<=' requires that operands are integers");
   
   // evacuate temps
   if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1){
      emit("", "mov", "["+contentsOfAReg+"],eax","; deassign AReg");
      symbolTable.at(contentsOfAReg).setAlloc(YES);
      symbolTable.at(contentsOfAReg).setValue("");
      contentsOfAReg = "";
   }
   
   if(!isTemporary(contentsOfAReg) && contentsOfAReg != operand1){
      // deassign
      contentsOfAReg = "";
   }
   // load operand if needed
   if(contentsOfAReg != operand1){
      // load operand 2 into register, arbitrary choice
      emit("","mov","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
      contentsOfAReg = operand1;
   }
   
   // perform comparison
   emit("","cmp","eax,["+symbolTable.at(operand2).getInternalName()+"]","; compare " + contentsOfAReg + " and " + operand2);
   string x = getLabel();
   emit("", "jle", x, "; if " + contentsOfAReg + " <= " + operand2 + " then jump to set eax to TRUE");
   emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");
   // if false doesn't exist, insert it
   if(symbolTable.find("false") == symbolTable.end()){
      symbolTable.insert(pair<string, SymbolTableEntry>("false",
                               SymbolTableEntry("FALSE",BOOLEAN,CONSTANT,"false",YES,1)));
   }
   string y = getLabel();
   emit("", "jmp", y, "; unconditionally jump");
   emit(x+":","","","");
   emit("","mov", "eax,[TRUE]", "; set eax to TRUE");
   // if true doesn't exist, insert it
   if(symbolTable.find("true") == symbolTable.end()){
      symbolTable.insert(pair<string, SymbolTableEntry>("true",
                               SymbolTableEntry("TRUE",BOOLEAN,CONSTANT,"true",YES,1)));
   }
   emit(y+":","","","");
   
   // deassign all temporaries
   if(isTemporary(operand1)){
		symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
	}
   if(isTemporary(operand2)){
		symbolTable.at(operand2).setValue("");
		symbolTable.at(operand2).setDataType(UNKNOWN);
		freeTemp();
	}
   
   string t = getTemp();
	symbolTable.at(t).setDataType(BOOLEAN);
	contentsOfAReg = t;
	pushOperand(t);
   
}

void Compiler::emitGreaterThanCode(string operand1, string operand2){
   // check for illegal types
   if(whichType(operand1) != INTEGER || INTEGER != whichType(operand2))
      processError("operator '>' requires that operands are integers");
   
   // evacuate temps
   if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1){
      emit("", "mov", "["+contentsOfAReg+"],eax","; deassign AReg");
      symbolTable.at(contentsOfAReg).setAlloc(YES);
      symbolTable.at(contentsOfAReg).setValue("");
      contentsOfAReg = "";
   }
   
   if(!isTemporary(contentsOfAReg) && contentsOfAReg != operand1){
      // deassign
      contentsOfAReg = "";
   }
   // load operand if needed
   if(contentsOfAReg != operand1){
      // load operand 2 into register, arbitrary choice
      emit("","mov","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
      contentsOfAReg = operand1;
   }
   
   // perform comparison
   emit("","cmp","eax,["+symbolTable.at(operand2).getInternalName()+"]","; compare " + contentsOfAReg + " and " + operand2);
   string x = getLabel();
   emit("", "jg", x, "; if " + contentsOfAReg + " > " + operand2 + " then jump to set eax to TRUE");
   emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");
   // if false doesn't exist, insert it
   if(symbolTable.find("false") == symbolTable.end()){
      symbolTable.insert(pair<string, SymbolTableEntry>("false",
                               SymbolTableEntry("FALSE",BOOLEAN,CONSTANT,"false",YES,1)));
   }
   string y = getLabel();
   emit("", "jmp", y, "; unconditionally jump");
   emit(x+":","","","");
   emit("","mov", "eax,[TRUE]", "; set eax to TRUE");
   // if true doesn't exist, insert it
   if(symbolTable.find("true") == symbolTable.end()){
      symbolTable.insert(pair<string, SymbolTableEntry>("true",
                               SymbolTableEntry("TRUE",BOOLEAN,CONSTANT,"true",YES,1)));
   }
   emit(y+":","","","");
   
   // deassign all temporaries
   if(isTemporary(operand1)){
		symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
	}
   if(isTemporary(operand2)){
		symbolTable.at(operand2).setValue("");
		symbolTable.at(operand2).setDataType(UNKNOWN);
		freeTemp();
	}
   
   string t = getTemp();
	symbolTable.at(t).setDataType(BOOLEAN);
	contentsOfAReg = t;
	pushOperand(t);
   
}

void Compiler::emitGreaterThanOrEqualToCode(string operand1, string operand2){
   // check for illegal types
   if(whichType(operand1) != INTEGER || INTEGER != whichType(operand2))
      processError("operator '>=' requires that operands are integers");
   
   // evacuate temps
   if(isTemporary(contentsOfAReg) && contentsOfAReg != operand1){
      emit("", "mov", "["+contentsOfAReg+"],eax","; deassign AReg");
      symbolTable.at(contentsOfAReg).setAlloc(YES);
      symbolTable.at(contentsOfAReg).setValue("");
      contentsOfAReg = "";
   }
   
   if(!isTemporary(contentsOfAReg) && contentsOfAReg != operand1){
      // deassign
      contentsOfAReg = "";
   }
   // load operand if needed
   if(contentsOfAReg != operand1){
      // load operand 2 into register, arbitrary choice
      emit("","mov","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
      contentsOfAReg = operand1;
   }
   
   // perform comparison
   emit("","cmp","eax,["+symbolTable.at(operand2).getInternalName()+"]","; compare " + contentsOfAReg + " and " + operand2);
   string x = getLabel();
   emit("", "jge", x, "; if " + contentsOfAReg + " >= " + operand2 + " then jump to set eax to TRUE");
   emit("", "mov", "eax,[FALSE]", "; else set eax to FALSE");
   // if false doesn't exist, insert it
   if(symbolTable.find("false") == symbolTable.end()){
      symbolTable.insert(pair<string, SymbolTableEntry>("false",
                               SymbolTableEntry("FALSE",BOOLEAN,CONSTANT,"false",YES,1)));
   }
   string y = getLabel();
   emit("", "jmp", y, "; unconditionally jump");
   emit(x+":","","","");
   emit("","mov", "eax,[TRUE]", "; set eax to TRUE");
   // if true doesn't exist, insert it
   if(symbolTable.find("true") == symbolTable.end()){
      symbolTable.insert(pair<string, SymbolTableEntry>("true",
                               SymbolTableEntry("TRUE",BOOLEAN,CONSTANT,"true",YES,1)));
   }
   emit(y+":","","","");
   
   // deassign all temporaries
   if(isTemporary(operand1)){
		symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
	}
   if(isTemporary(operand2)){
		symbolTable.at(operand2).setValue("");
		symbolTable.at(operand2).setDataType(UNKNOWN);
		freeTemp();
	}
   
   string t = getTemp();
	symbolTable.at(t).setDataType(BOOLEAN);
	contentsOfAReg = t;
	pushOperand(t);
   
}

// Stage 2 Emits

void Compiler::emitThenCode(string operand1, string operand2){
   string tempLabel;
   // make sure our predicate is a boolean
   if(whichType(operand1) != BOOLEAN)
      processError("if predicate must be of type boolean");
   // get a new label
   tempLabel = getLabel();
   // load our predicate if necessary
   if(contentsOfAReg != operand1)
      emit("","mov","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
   emit("", "cmp", "eax,0", "; compare eax to 0");
   emit("", "je", tempLabel, "; if " + operand1 + " is false then jump to end of if");
   
   // push the label awaiting else or postIfCode
   pushOperand(tempLabel);
   
   // free any used temp
   if(isTemporary(operand1)){
		symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
	}
   contentsOfAReg = "";
}

void Compiler::emitElseCode(string operand1, string operand2){
   string tempLabel;
   tempLabel = getLabel();
   emit("", "jmp", tempLabel, "; jump to end if");
   emit(operand1+":", "", "", "; else");
   pushOperand(tempLabel);
   contentsOfAReg = "";
};

void Compiler::emitPostIfCode(string operand1, string operand2){
   emit(operand1+":","","","; end if");
   contentsOfAReg = "";
};

void Compiler::emitWhileCode(string operand1, string operand2){
   string tempLabel;
   tempLabel = getLabel();
   emit(tempLabel+":","","","; while");
   pushOperand(tempLabel);
   contentsOfAReg = "";
};

void Compiler::emitDoCode(string operand1, string operand2){
   string tempLabel;
   // make sure our predicate is a boolean
   if(whichType(operand1) != BOOLEAN)
      processError("if predicate must be of type boolean");
   // get a new label
   tempLabel = getLabel();
   // load our predicate if necessary
   if(contentsOfAReg != operand1)
      emit("","mov","eax,["+symbolTable.at(operand1).getInternalName()+"]", "; AReg = " + operand1);
   emit("", "cmp", "eax,0", "; compare eax to 0");
   emit("", "je", tempLabel, "; if " + operand1 + " is false then jump to end while");
   
   // push the label awaiting postWhileCode
   pushOperand(tempLabel);
   
   // free any used temp
   if(isTemporary(operand1)){
		symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
	}
   contentsOfAReg = "";
}

void Compiler::emitPostWhileCode(string operand1, string operand2){
   emit("","jmp",operand1,"; end while");
   emit(operand2+":","","");
   contentsOfAReg = "";
};

void Compiler::emitRepeatCode(string operand1, string operand2){
   string tempLabel;
   tempLabel = getLabel();
   emit(tempLabel+":","","","; repeat");
   pushOperand(tempLabel);
   contentsOfAReg = "";
}

void Compiler::emitUntilCode(string operand1, string operand2){
   // make sure our predicate is a boolean
   if(whichType(operand2) != BOOLEAN)
      processError("if predicate must be of type boolean");
   // load our predicate if necessary
   if(contentsOfAReg != operand2)
      emit("","mov","eax,["+symbolTable.at(operand2).getInternalName()+"]", "; AReg = " + operand1);
   emit("", "cmp", "eax,0", "; compare eax to 0");
   emit("", "je", operand1, "; until " + operand2 + " is true");
   
   // free any used temp
   if(isTemporary(operand1)){
		symbolTable.at(operand1).setValue("");
		symbolTable.at(operand1).setDataType(UNKNOWN);
		freeTemp();
	}
   contentsOfAReg = "";
}



/* -----------
LEXICAL ROUTINES
*/

char Compiler::nextChar(){
   sourceFile.get(ch);
   if(sourceFile.eof()){
      ch = END_OF_FILE;
      return ch;
   }
   // https://cplusplus.com/reference/cstdio/EOF/
   // will generally return -1 upon EOF
   if(ch == '\n' && sourceFile.peek() != -1){
      lineNo++;
      listingFile << endl << right << setw(6) << to_string(lineNo) + "|";
   }else{
      listingFile << left << ch;
   }
   return ch;
}

string Compiler::nextToken(){
   token = "";
   while (token == ""){
         
         if (ch == '{'){ // process comment (if EOFs all the time, this is an error)
            
            while(nextChar() != '}' && ch != END_OF_FILE){}
            if (ch == END_OF_FILE)
               processError("unexpected end of file");
            else
               nextChar();
            
         }else if(ch == '}'){
            processError("\'}\' cannot begin token");
         }else if(isspace(ch)){
            nextChar();
         }else if(isSpecialSymbol(ch)){
            token = ch;
            nextChar();
            // process := tokens
            if(token == ":" && ch == '='){
               token += ch;
               nextChar();
            }
            // process <>, <=, >= tokens
            if(token == "<"){
               if(ch == '>' || ch == '='){
                  token += ch;
                  nextChar();
               }
            }else if(token == ">"){
               if(ch == '='){
                  token += ch;
                  nextChar();
               }
            }
         }else if(islower(ch)){
            token = ch;
            while((isalpha(nextChar()) || isdigit(ch) || ch == '_') && ch != END_OF_FILE){
               token += ch;
            }
            if(ch == END_OF_FILE)
               processError("unexpected end of file");
         }else if(isdigit(ch)){
            token = ch;
            while(isdigit(nextChar()) && ch != END_OF_FILE){
               token += ch;
            }
            if (ch == END_OF_FILE)
               processError("unexpected end of file");
         }else if(ch == END_OF_FILE){
            token = ch;
         }else{
            processError("illegal symbol");
         }
         
   }
   return token;
}

/* -----------
OTHER ROUTINES
*/

string Compiler::genInternalName(storeTypes stype) const{
   static int curbool = 0;
   static int curint = 0;
   string out;
   if(stype == BOOLEAN){
      out = "B"+to_string(curbool);
      curbool++;
   }else if(stype == INTEGER){
      out = "I"+to_string(curint);
      curint++;
   }
   return out;
}

void Compiler::processError(string err){
   listingFile << endl;
   listingFile << "\nError: Line " << to_string(lineNo) << ": " << err << endl;
   errorCount++;
   createListingTrailer();
   exit(-1);
}

// Stage 1 Additions
void Compiler::freeTemp(){
	currentTempNo--;
	if(currentTempNo < -1)
		processError("compiler error, currentTempNo should be >= -1");
}

string Compiler::getTemp(){
	string temp;
	currentTempNo++;
	temp = "T" + to_string(currentTempNo);
	if(currentTempNo > maxTempNo){
		insert(temp, UNKNOWN, VARIABLE, "", NO, 1);
		maxTempNo++;
	}
	return temp;
}

string Compiler::getLabel(){
   // i couldn't think of a way to keep this data besides a static
   static int labelNo = 0;
   string L = ".L"+to_string(labelNo);
   labelNo++;
   return L;
}

bool Compiler::isTemporary(string s) const{
   // sneaky bug :D
   // Internal name for 'true' is TRUE
   // it gets freed as if its a temp if these two lines don't exist
   if(s == "true")
      return false;
   
	auto it = symbolTable.find(s);
	if(it == symbolTable.end()){
		return false;
	}
	if(it->second.getInternalName()[0] == 'T'){
		return true;
	}
	return false;
}

bool Compiler::isLabel(string s) const{
   // labels are ".L[0-9]+
   // external names for variables never have a period at the beginning
   if(s.at(0) != '.')
      return false;
   // and this might not even really be super necessary
   if(s.at(1) != 'L')
      return false;
   // this should be good enough
   // did not need this function at all so probably did something wrong.
   return true;
}
// under 2000 :)