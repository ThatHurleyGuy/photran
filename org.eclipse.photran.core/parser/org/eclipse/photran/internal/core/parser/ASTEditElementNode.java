// Generated by Ludwig version 1.0 alpha 6

package org.eclipse.photran.internal.core.parser; import org.eclipse.photran.internal.core.lexer.*;


/**
 * <EditElement> ::= tfcon:T_FCON  :production808
 * <EditElement> ::= tscon:T_SCON  :production809
 * <EditElement> ::= tident:T_IDENT  :production810
 * <EditElement> ::= tlparen:T_LPAREN FmtSpec:<FmtSpec> trparen:T_RPAREN  :production811
 * <EditElement> ::= thcon:T_HCON  :production812
 */
public class ASTEditElementNode extends ParseTreeNode
{
    public ASTEditElementNode(Nonterminal nonterminal, Production production)
    {
        super(nonterminal, production);
    }

    public Token getASTTfcon()
    {
        return this.getChildToken("tfcon");
    }

    public Token getASTTscon()
    {
        return this.getChildToken("tscon");
    }

    public Token getASTTident()
    {
        return this.getChildToken("tident");
    }

    public Token getASTTlparen()
    {
        return this.getChildToken("tlparen");
    }

    public ASTFmtSpecNode getASTFmtSpec()
    {
        return (ASTFmtSpecNode)this.getChild("FmtSpec");
    }

    public Token getASTTrparen()
    {
        return this.getChildToken("trparen");
    }

    public Token getASTThcon()
    {
        return this.getChildToken("thcon");
    }

    protected void visitThisNodeUsing(ASTVisitor visitor) { visitor.visitASTEditElementNode(this); }
}