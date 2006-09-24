// Generated by Ludwig version 1.0 alpha 6

package org.eclipse.photran.internal.core.parser; import org.eclipse.photran.internal.core.lexer.*;


/**
 * <PrivateSequenceStmt> ::= LblDef:<LblDef> tprivate:T_PRIVATE teos:T_EOS  :production187
 * <PrivateSequenceStmt> ::= LblDef:<LblDef> tsequence:T_SEQUENCE teos:T_EOS  :production188
 */
public class ASTPrivateSequenceStmtNode extends ParseTreeNode
{
    public ASTPrivateSequenceStmtNode(Nonterminal nonterminal, Production production)
    {
        super(nonterminal, production);
    }

    public ASTLblDefNode getASTLblDef()
    {
        return (ASTLblDefNode)this.getChild("LblDef");
    }

    public Token getASTTprivate()
    {
        return this.getChildToken("tprivate");
    }

    public Token getASTTeos()
    {
        return this.getChildToken("teos");
    }

    public Token getASTTsequence()
    {
        return this.getChildToken("tsequence");
    }

    protected void visitThisNodeUsing(ASTVisitor visitor) { visitor.visitASTPrivateSequenceStmtNode(this); }
}