package org.eclipse.photran.internal.core.parser;

import org.eclipse.photran.internal.core.lexer.Token;
import org.eclipse.photran.internal.core.parser.Parser.*;
import java.util.List;

public class ASTArrayDeclaratorListNode extends InteriorNode
{
    protected int count = -1;

    ASTArrayDeclaratorListNode(Production production, List<CSTNode> childNodes, List<CSTNode> discardedSymbols)
    {
         super(production);
         
         for (Object o : childNodes)
             addChild((CSTNode)o);
         constructionFinished();
    }

    /**
     * @return the number of ASTArrayDeclaratorListNode nodes in this list
     */
    public int size()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods, including size(), cannot be called on the nodes of a CST after it has been modified");
        
        if (count >= 0) return count;
        
        count = 0;
        ASTArrayDeclaratorListNode node = this;
        do
        {
            count++;
            node = node.getRecursiveNode();
        }
        while (node != null);
        
        return count;
    }
    
    ASTArrayDeclaratorListNode recurseToIndex(int listIndex)
    {
        ASTArrayDeclaratorListNode node = this;
        for (int depth = size()-listIndex-1, i = 0; i < depth; i++)
        {
            if (node == null) throw new IllegalArgumentException("Index " + listIndex + " out of bounds (size: " + size() + ")");
            node = (ASTArrayDeclaratorListNode)node.getRecursiveNode();
        }
        return node;
    }
    
    @Override protected void visitThisNodeUsing(ASTVisitor visitor)
    {
        visitor.visitASTArrayDeclaratorListNode(this);
    }

    public ASTArrayDeclaratorNode getArrayDeclarator(int listIndex)
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        ASTArrayDeclaratorListNode node = recurseToIndex(listIndex);
        if (node.getProduction() == Production.ARRAY_DECLARATOR_LIST_341)
            return (ASTArrayDeclaratorNode)node.getChild(0);
        else if (node.getProduction() == Production.ARRAY_DECLARATOR_LIST_342)
            return (ASTArrayDeclaratorNode)node.getChild(2);
        else
            return null;
    }

    private ASTArrayDeclaratorListNode getRecursiveNode()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.ARRAY_DECLARATOR_LIST_342)
            return (ASTArrayDeclaratorListNode)getChild(0);
        else
            return null;
    }

    public Token getTComma(int listIndex)
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        ASTArrayDeclaratorListNode node = recurseToIndex(listIndex);
        if (node.getProduction() == Production.ARRAY_DECLARATOR_LIST_342)
            return (Token)node.getChild(1);
        else
            return null;
    }
}
