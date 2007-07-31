package org.eclipse.photran.internal.core.parser;

import org.eclipse.photran.internal.core.lexer.Token;
import org.eclipse.photran.internal.core.parser.Parser.*;
import java.util.List;

public class ASTDataIDoObjectListNode extends InteriorNode
{
    protected int count = -1;

    ASTDataIDoObjectListNode(Production production, List<CSTNode> childNodes, List<CSTNode> discardedSymbols)
    {
         super(production);
         
         for (Object o : childNodes)
             addChild((CSTNode)o);
         constructionFinished();
    }

    /**
     * @return the number of ASTDataIDoObjectListNode nodes in this list
     */
    public int size()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods, including size(), cannot be called on the nodes of a CST after it has been modified");
        
        if (count >= 0) return count;
        
        count = 0;
        ASTDataIDoObjectListNode node = this;
        do
        {
            count++;
            node = node.getRecursiveNode();
        }
        while (node != null);
        
        return count;
    }
    
    ASTDataIDoObjectListNode recurseToIndex(int listIndex)
    {
        ASTDataIDoObjectListNode node = this;
        for (int depth = size()-listIndex-1, i = 0; i < depth; i++)
        {
            if (node == null) throw new IllegalArgumentException("Index " + listIndex + " out of bounds (size: " + size() + ")");
            node = (ASTDataIDoObjectListNode)node.getRecursiveNode();
        }
        return node;
    }
    
    @Override protected void visitThisNodeUsing(ASTVisitor visitor)
    {
        visitor.visitASTDataIDoObjectListNode(this);
    }

    public ASTDataIDoObjectNode getDataIDoObject(int listIndex)
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        ASTDataIDoObjectListNode node = recurseToIndex(listIndex);
        if (node.getProduction() == Production.DATA_IDO_OBJECT_LIST_379)
            return (ASTDataIDoObjectNode)node.getChild(0);
        else if (node.getProduction() == Production.DATA_IDO_OBJECT_LIST_380)
            return (ASTDataIDoObjectNode)node.getChild(2);
        else
            return null;
    }

    private ASTDataIDoObjectListNode getRecursiveNode()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.DATA_IDO_OBJECT_LIST_380)
            return (ASTDataIDoObjectListNode)getChild(0);
        else
            return null;
    }

    public Token getTComma(int listIndex)
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        ASTDataIDoObjectListNode node = recurseToIndex(listIndex);
        if (node.getProduction() == Production.DATA_IDO_OBJECT_LIST_380)
            return (Token)node.getChild(1);
        else
            return null;
    }
}
