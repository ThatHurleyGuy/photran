package org.eclipse.photran.internal.core.parser;

import org.eclipse.photran.internal.core.lexer.Token;
import org.eclipse.photran.internal.core.parser.Parser.*;
import java.util.List;

public class ASTEndDoStmtNode extends InteriorNode
{
    ASTEndDoStmtNode(Production production, List<CSTNode> childNodes, List<CSTNode> discardedSymbols)
    {
         super(production);
         
         for (Object o : childNodes)
             addChild((CSTNode)o);
         constructionFinished();
    }
    
    @Override protected void visitThisNodeUsing(ASTVisitor visitor)
    {
        visitor.visitASTEndDoStmtNode(this);
    }

    public ASTLblDefNode getLblDef()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.END_DO_STMT_715)
            return (ASTLblDefNode)getChild(0);
        else if (getProduction() == Production.END_DO_STMT_716)
            return (ASTLblDefNode)getChild(0);
        else if (getProduction() == Production.END_DO_STMT_717)
            return (ASTLblDefNode)getChild(0);
        else if (getProduction() == Production.END_DO_STMT_718)
            return (ASTLblDefNode)getChild(0);
        else
            return null;
    }

    public Token getTEnddo()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.END_DO_STMT_715)
            return (Token)getChild(1);
        else if (getProduction() == Production.END_DO_STMT_716)
            return (Token)getChild(1);
        else
            return null;
    }

    public Token getTEos()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.END_DO_STMT_715)
            return (Token)getChild(2);
        else if (getProduction() == Production.END_DO_STMT_716)
            return (Token)getChild(3);
        else if (getProduction() == Production.END_DO_STMT_717)
            return (Token)getChild(3);
        else if (getProduction() == Production.END_DO_STMT_718)
            return (Token)getChild(4);
        else
            return null;
    }

    public ASTEndNameNode getEndName()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.END_DO_STMT_716)
            return (ASTEndNameNode)getChild(2);
        else if (getProduction() == Production.END_DO_STMT_718)
            return (ASTEndNameNode)getChild(3);
        else
            return null;
    }

    public Token getTEnd()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.END_DO_STMT_717)
            return (Token)getChild(1);
        else if (getProduction() == Production.END_DO_STMT_718)
            return (Token)getChild(1);
        else
            return null;
    }

    public Token getTDo()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.END_DO_STMT_717)
            return (Token)getChild(2);
        else if (getProduction() == Production.END_DO_STMT_718)
            return (Token)getChild(2);
        else
            return null;
    }
}
