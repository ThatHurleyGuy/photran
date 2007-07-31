package org.eclipse.photran.internal.core.parser;

import org.eclipse.photran.internal.core.lexer.Token;
import org.eclipse.photran.internal.core.parser.Parser.*;
import java.util.List;

public class ASTFunctionSubprogramNode extends InteriorNode
{
    ASTFunctionSubprogramNode(Production production, List<CSTNode> childNodes, List<CSTNode> discardedSymbols)
    {
         super(production);
         
         for (Object o : childNodes)
             addChild((CSTNode)o);
         constructionFinished();
    }
    
    @Override protected void visitThisNodeUsing(ASTVisitor visitor)
    {
        visitor.visitASTFunctionSubprogramNode(this);
    }

    public ASTFunctionStmtNode getFunctionStmt()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.FUNCTION_SUBPROGRAM_17)
            return (ASTFunctionStmtNode)getChild(0);
        else
            return null;
    }

    public ASTBodyNode getBody()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.FUNCTION_SUBPROGRAM_17)
            return (ASTBodyNode)getChild(1, 0);
        else
            return null;
    }

    public ASTEndFunctionStmtNode getEndFunctionStmt()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.FUNCTION_SUBPROGRAM_17)
            return (ASTEndFunctionStmtNode)getChild(1, 1);
        else if (getProduction() == Production.FUNCTION_SUBPROGRAM_17)
            return (ASTEndFunctionStmtNode)getChild(1, 0);
        else if (getProduction() == Production.FUNCTION_SUBPROGRAM_17)
            return (ASTEndFunctionStmtNode)getChild(1, 1);
        else
            return null;
    }

    public ASTBodyPlusInternalsNode getBodyPlusInternals()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.FUNCTION_SUBPROGRAM_17)
            return (ASTBodyPlusInternalsNode)getChild(1, 0);
        else
            return null;
    }

    @Override protected boolean childIsPulledUp(int index)
    {
        if (getProduction() == Production.FUNCTION_SUBPROGRAM_17 && index == 1)
            return true;
        else
            return false;
    }
}
