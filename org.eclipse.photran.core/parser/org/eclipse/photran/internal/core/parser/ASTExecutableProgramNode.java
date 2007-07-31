package org.eclipse.photran.internal.core.parser;

import org.eclipse.photran.internal.core.lexer.Token;
import org.eclipse.photran.internal.core.parser.Parser.*;
import java.util.List;

public class ASTExecutableProgramNode extends InteriorNode
{
    protected int count = -1;

    ASTExecutableProgramNode(Production production, List<CSTNode> childNodes, List<CSTNode> discardedSymbols)
    {
         super(production);
         
         for (Object o : childNodes)
             addChild((CSTNode)o);
         constructionFinished();
    }

    /**
     * @return the number of ASTExecutableProgramNode nodes in this list
     */
    public int size()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods, including size(), cannot be called on the nodes of a CST after it has been modified");
        
        if (count >= 0) return count;
        
        count = 0;
        ASTExecutableProgramNode node = this;
        do
        {
            count++;
            node = node.getRecursiveNode();
        }
        while (node != null);
        
        return count;
    }
    
    ASTExecutableProgramNode recurseToIndex(int listIndex)
    {
        ASTExecutableProgramNode node = this;
        for (int depth = size()-listIndex-1, i = 0; i < depth; i++)
        {
            if (node == null) throw new IllegalArgumentException("Index " + listIndex + " out of bounds (size: " + size() + ")");
            node = (ASTExecutableProgramNode)node.getRecursiveNode();
        }
        return node;
    }
    
    @Override protected void visitThisNodeUsing(ASTVisitor visitor)
    {
        visitor.visitASTExecutableProgramNode(this);
    }

    public ASTProgramUnitNode getProgramUnit(int listIndex)
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        ASTExecutableProgramNode node = recurseToIndex(listIndex);
        if (node.getProduction() == Production.EXECUTABLE_PROGRAM_1)
            return (ASTProgramUnitNode)node.getChild(0);
        else if (node.getProduction() == Production.EXECUTABLE_PROGRAM_2)
            return (ASTProgramUnitNode)node.getChild(1);
        else
            return null;
    }

    private ASTExecutableProgramNode getRecursiveNode()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.EXECUTABLE_PROGRAM_2)
            return (ASTExecutableProgramNode)getChild(0);
        else
            return null;
    }
}
