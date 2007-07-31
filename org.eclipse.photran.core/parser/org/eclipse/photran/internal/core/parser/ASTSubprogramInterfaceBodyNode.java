package org.eclipse.photran.internal.core.parser;

import org.eclipse.photran.internal.core.lexer.Token;
import org.eclipse.photran.internal.core.parser.Parser.*;
import java.util.List;

public class ASTSubprogramInterfaceBodyNode extends InteriorNode
{
    ASTSubprogramInterfaceBodyNode(Production production, List<CSTNode> childNodes, List<CSTNode> discardedSymbols)
    {
         super(production);
         
         for (Object o : childNodes)
             addChild((CSTNode)o);
         constructionFinished();
    }
    
    @Override protected void visitThisNodeUsing(ASTVisitor visitor)
    {
        visitor.visitASTSubprogramInterfaceBodyNode(this);
    }

    public ASTSpecificationPartConstructNode getSpecificationPartConstruct()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.SUBPROGRAM_INTERFACE_BODY_942)
            return (ASTSpecificationPartConstructNode)getChild(0);
        else if (getProduction() == Production.SUBPROGRAM_INTERFACE_BODY_943)
            return (ASTSpecificationPartConstructNode)getChild(1);
        else
            return null;
    }

    public ASTSubprogramInterfaceBodyNode getSubprogramInterfaceBody()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.SUBPROGRAM_INTERFACE_BODY_943)
            return (ASTSubprogramInterfaceBodyNode)getChild(0);
        else
            return null;
    }
}
