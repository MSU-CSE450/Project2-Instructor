#include <assert.h>
#include <fstream>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include "ASTNode.hpp"
#include "lexer.hpp"
#include "SymbolTable.hpp"
#include "TokenQueue.hpp"

class MACROCalc {
private:
  TokenQueue tokens;
  SymbolTable symbols{};
  ASTNode_Block root{0};

  // Dynamic system to handle operator precedence and associativity.
  struct OpInfo {
    size_t level;
    char assoc;   // l=left; r=right; n=non-associative
  };
  std::unordered_map<std::string, OpInfo> op_map{};

  // Abstract syntax tree nodes should be passed around as unique pointers.
  using ast_ptr_t = std::unique_ptr<ASTNode>;

  // == HELPER FUNCTIONS

  // Trigger an error at the position of the current token (or previous if none are left)
  template <typename... Ts>
  void TriggerError(Ts... message) {
    if (tokens.None()) tokens.Rewind();
    Error(tokens.CurLine(), std::forward<Ts>(message)...);
  }

  // Create a unique pointer for a node of a specified type.
  template <typename NODE_T, typename... ARG_Ts>
  ast_ptr_t MakeNode(ARG_Ts &&... args) {
    return std::make_unique<NODE_T>( std::forward<ARG_Ts>(args)... );
  }

  // Create a unique pointer for a variable (with the provided identifier token in formation)
  ast_ptr_t MakeVarNode(emplex::Token token) {
    if (!symbols.Has(token.lexeme)) {
      TriggerError("Using unknown variable '", token.lexeme, "'");
    }
    return MakeNode<ASTNode_Var>(token, symbols);
  }

  // Setup the precedence and associativity of every operator.
  void SetupOperators() {
    // Go from highest precedence to lowest.
    size_t cur_prec = 0;
    op_map["!"] =                                             OpInfo{cur_prec++, 'n'};
    op_map["**"] =                                            OpInfo{cur_prec++, 'r'};
    op_map["*"] = op_map["/"]  = op_map["%"] =                OpInfo{cur_prec++, 'l'};
    op_map["+"] = op_map["-"]  =                              OpInfo{cur_prec++, 'l'};
    op_map["<"] = op_map["<="] = op_map[">"] = op_map[">="] = OpInfo{cur_prec++, 'n'};
    op_map["=="] = op_map["!="] =                             OpInfo{cur_prec++, 'n'};
    op_map["&&"] =                                            OpInfo{cur_prec++, 'l'};
    op_map["||"] =                                            OpInfo{cur_prec++, 'l'};
    op_map["="] =                                             OpInfo{cur_prec++, 'r'};
  }

public:
  MACROCalc(std::string filename) {    
    std::ifstream in_file(filename);              // Load the input file
    if (in_file.fail()) {
      std::cout << "ERROR: Unable to open file '" << filename << "'." << std::endl;
      exit(1);
    }

    tokens.Load(in_file);  // Load tokens from the file.
  }

  // Convert any token representing a unary value into an ASTNode.
  // (i.e., a leaf in an expression and associated unary operators)
  ast_ptr_t Parse_UnaryTerm() {
    const emplex::Token & token = tokens.Use();

    // Check operator type
    switch (token.id) {
    case '+':   // Skip any operation.
      return Parse_UnaryTerm();
    case '-':   // Attach unary operators to the term that follows.
    case '!':
      return MakeNode<ASTNode_Math1>(token, Parse_UnaryTerm());
    case '(': {  // Allow full expressions in parentheses.
      auto out = Parse_Expression();
      tokens.Use(')');
      return out;
    }
    case emplex::Lexer::ID_ID:
      return MakeVarNode(token);
    case emplex::Lexer::ID_LIT_VALUE:
      return MakeNode<ASTNode_NumLit>(token.line_id, std::stod(token.lexeme));
    default:
      TriggerError("Unexpected token '", token.lexeme, "'");
    }

    return nullptr;
  }

  // Parse expressions.  The level input determines how restrictive this parse should be.
  // Only continue processing with types at the target level or higher.
  ast_ptr_t Parse_Expression(size_t prec_limit=1000) {
    // Any expression must begin with a variable name or a literal value.
    ast_ptr_t cur_node = Parse_UnaryTerm();

    size_t skip_prec = 1000; // If we get a non-associative op, we must skip next one.

    // While there are more tokens to process, try to expand this expression.
    while (tokens.Any()) {
      // Peek at the next token; if it is an op, keep going and get its info.
      auto op_token = tokens.Peek();
      if (!op_map.count(op_token.lexeme)) break;  // Not an op token; stop here!
      OpInfo op_info = op_map[op_token.lexeme];

      // If precedence of next operator is too high, return what we have.
      if (op_info.level > prec_limit) break;

      // If the next precedence is not allowed, throw an error.
      if (op_info.level == skip_prec) {
        TriggerError("Operator '", op_token.lexeme, "' is non-associative.");
      }

      // If we made it here, we have a binary operation to use, so consume it.
      tokens.Use();

      // Find the allowed precedence for the next term.
      size_t next_limit = op_info.level;
      if (op_info.assoc != 'r') --next_limit;

      // Load the next term.
      ast_ptr_t node2 = Parse_Expression(next_limit);

      cur_node = MakeNode<ASTNode_Math2>(op_token, std::move(cur_node), std::move(node2));

      skip_prec = (op_info.assoc == 'n') ? op_info.level : 1000;
    }

    return cur_node;
  }

  ast_ptr_t Parse_Statement() {
    // Test what kind of statement this is and call the appropriate function...
    switch (tokens.Peek()) {
      using namespace emplex;
      case Lexer::ID_VAR:   return Parse_Statement_Var();
      case Lexer::ID_IF:    return Parse_Statement_If();
      case Lexer::ID_WHILE: return Parse_Statement_While();
      case Lexer::ID_PRINT: return Parse_Statement_Print();
      case '{': return Parse_StatementList();
      case ';':
        tokens.Use();
        return nullptr;
      default: return Parse_Statement_Expression();
    }
  }

  ast_ptr_t Parse_Statement_Var() {
     // Variable declarations shouldn't be called without 'var' keyword.
    tokens.Use(emplex::Lexer::ID_VAR);
    const auto var_token =
      tokens.Use(emplex::Lexer::ID_ID,
        "Keyword 'var' must be followed by identifier; found '",
        tokens.Peek().lexeme, "'.");
    symbols.AddVar(var_token.lexeme, var_token.line_id);
    if (tokens.UseIf(';')) {
      return nullptr;  // Variable added, nothing else to do.
    }
    tokens.Use('=', "Expected ';' or '=' after var declaration of '", var_token.lexeme,
      "'; found '", tokens.Peek().lexeme, "'.");
    auto expr_node = Parse_Expression();
    tokens.Use(';');
    return MakeNode<ASTNode_Math2>(var_token.line_id, "=", MakeVarNode(var_token), std::move(expr_node));
  }

  ast_ptr_t Parse_Statement_If() {
    size_t line_num = tokens.Use(emplex::Lexer::ID_IF).line_id;
    tokens.Use('(', "If commands must be followed by a '('; found '", tokens.Peek().lexeme, "'.");
    ast_ptr_t condition = Parse_Expression();
    tokens.Use(')');
    ast_ptr_t action = Parse_Statement();

    // Check if we need to add on an "else" branch
    if (tokens.UseIf(emplex::Lexer::ID_ELSE)) {
      ast_ptr_t alt = Parse_Statement();
      return MakeNode<ASTNode_If>(line_num, std::move(condition), std::move(action), std::move(alt));
    }

    return MakeNode<ASTNode_If>(line_num, std::move(condition), std::move(action));
  }

  ast_ptr_t Parse_Statement_While() {
    size_t line_num = tokens.Use(emplex::Lexer::ID_WHILE).line_id;
    tokens.Use('(', "While commands must be followed by a '(");
    ast_ptr_t condition = Parse_Expression();
    tokens.Use(')');
    ast_ptr_t action = Parse_Statement();
    return MakeNode<ASTNode_While>(line_num, std::move(condition), std::move(action));
  }

  ast_ptr_t Parse_Statement_Print() {
    size_t line_num = tokens.Use(emplex::Lexer::ID_PRINT).line_id;
    tokens.Use('(', "Print commands must be followed by a '(");
    ast_ptr_t out_ptr;
    if (tokens.Is(emplex::Lexer::ID_LIT_STRING)) {
      out_ptr = MakeNode<ASTNode_Print>(line_num, tokens.Use().lexeme, symbols);
    } else {
      out_ptr = MakeNode<ASTNode_Print>(line_num, Parse_Expression());
    }
    tokens.Use(')');
    tokens.Use(';');
    return out_ptr;
  }

  ast_ptr_t Parse_Statement_Expression() {
    ast_ptr_t out = Parse_Expression();
    tokens.Use(';');
    return out;
  }

  ast_ptr_t Parse_StatementList() {
    auto out_node = std::make_unique<ASTNode_Block>(tokens.Peek().line_id);
    tokens.Use('{', "Statement blocks must start with '{'.");
    symbols.PushScope();
    while (tokens.Any() && tokens.Peek() != '}') {
      ast_ptr_t statement = Parse_Statement();
      if (statement) out_node->AddChild( std::move(statement) );
    }
    symbols.PopScope();
    tokens.Use('}', "Statement blocks must end with '}'.");
    return out_node;
  }

  void Parse() {
    SetupOperators();                          // Setup the operators for parsing.

    // Continually parse statements and add them to the root as long as tokens remain.
    while (tokens.Any()) {
      ast_ptr_t node = Parse_Statement();
      if (node) root.AddChild(std::move(node));
    }
  }

  void Run() {
    // All statements were added to the root; let it run them all.
    root.Run(symbols);
  }

  // For debugging, print all symbols int he Symbol Table
  void PrintSymbols() const { symbols.Print(); }

  // For debugging, print out the Abstract Syntax Tree
  void PrintAST() const { root.Print(); }
};


int main(int argc, char * argv[])
{
  if (argc != 2) {
    std::cout << "Format: " << argv[0] << " [filename]" << std::endl;
    exit(1);
  }


  MACROCalc prog(argv[1]);
  prog.Parse();

  // prog.PrintSymbols();  // DEBUGGING!
  // prog.PrintAST();      // DEBUGGING!

  prog.Run();
}
