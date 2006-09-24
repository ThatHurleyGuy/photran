// Generated by Ludwig version 1.0 alpha 6

package org.eclipse.photran.internal.core.parser; import org.eclipse.photran.internal.core.lexer.*;


/**
 * <SubprogramInterfaceBody> ::= SpecificationPartConstruct:<SpecificationPartConstruct>  :production858
 * <SubprogramInterfaceBody> ::= SubprogramInterfaceBody:<SubprogramInterfaceBody> SpecificationPartConstruct:<SpecificationPartConstruct>  :production859
 */
public class ASTSubprogramInterfaceBodyNode extends ParseTreeNode
{
    public ASTSubprogramInterfaceBodyNode(Nonterminal nonterminal, Production production)
    {
        super(nonterminal, production);
    }

    public ASTSpecificationPartConstructNode getASTSpecificationPartConstruct()
    {
        return (ASTSpecificationPartConstructNode)this.getChild("SpecificationPartConstruct");
    }

    public ASTSubprogramInterfaceBodyNode getASTSubprogramInterfaceBody()
    {
        return (ASTSubprogramInterfaceBodyNode)this.getChild("SubprogramInterfaceBody");
    }

    protected void visitThisNodeUsing(ASTVisitor visitor) { visitor.visitASTSubprogramInterfaceBodyNode(this); }
}