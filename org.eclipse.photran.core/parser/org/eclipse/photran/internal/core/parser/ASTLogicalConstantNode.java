// Generated by Ludwig version 1.0 alpha 6

package org.eclipse.photran.internal.core.parser; import org.eclipse.photran.internal.core.lexer.*;


/**
 * <LogicalConstant> ::= ttrue:T_TRUE  :production175
 * <LogicalConstant> ::= tfalse:T_FALSE  :production176
 * <LogicalConstant> ::= ttrue:T_TRUE tunderscore:T_UNDERSCORE KindParam:<KindParam>  :production177
 * <LogicalConstant> ::= tfalse:T_FALSE tunderscore:T_UNDERSCORE KindParam:<KindParam>  :production178
 */
public class ASTLogicalConstantNode extends ParseTreeNode
{
    public ASTLogicalConstantNode(Nonterminal nonterminal, Production production)
    {
        super(nonterminal, production);
    }

    public Token getASTTtrue()
    {
        return this.getChildToken("ttrue");
    }

    public Token getASTTfalse()
    {
        return this.getChildToken("tfalse");
    }

    public Token getASTTunderscore()
    {
        return this.getChildToken("tunderscore");
    }

    public ASTKindParamNode getASTKindParam()
    {
        return (ASTKindParamNode)this.getChild("KindParam");
    }

    protected void visitThisNodeUsing(ASTVisitor visitor) { visitor.visitASTLogicalConstantNode(this); }
}