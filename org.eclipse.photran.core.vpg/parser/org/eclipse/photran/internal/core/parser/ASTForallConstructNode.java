/*******************************************************************************
 * Copyright (c) 2007 University of Illinois at Urbana-Champaign and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     UIUC - Initial API and implementation
 *******************************************************************************/
package org.eclipse.photran.internal.core.parser;

import org.eclipse.photran.internal.core.lexer.*;                   import org.eclipse.photran.internal.core.analysis.binding.ScopingNode;

import org.eclipse.photran.internal.core.parser.Parser.*;
import java.util.List;

public class ASTForallConstructNode extends InteriorNode implements IExecutableConstruct, IForallBodyConstruct
{
    ASTForallConstructNode(Production production, List<CSTNode> childNodes, List<CSTNode> discardedSymbols)
    {
         super(production);
         
         for (Object o : childNodes)
             addChild((CSTNode)o);
         constructionFinished();
    }
        
    @Override public InteriorNode getASTParent()
    {
        InteriorNode actualParent = super.getParent();
        
        // If a node has been pulled up in an ACST, its physical parent in
        // the CST is not its logical parent in the ACST
        if (actualParent != null && actualParent.childIsPulledUp(actualParent.findChild(this)))
            return actualParent.getParent();
        else 
            return actualParent;
    }
    
    @Override protected void visitThisNodeUsing(ASTVisitor visitor)
    {
        visitor.visitIExecutableConstruct(this);
        visitor.visitIForallBodyConstruct(this);
        visitor.visitASTForallConstructNode(this);
    }

    public ASTForallConstructStmtNode getForallConstructStmt()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.FORALL_CONSTRUCT_631)
            return (ASTForallConstructStmtNode)getChild(0);
        else if (getProduction() == Production.FORALL_CONSTRUCT_632)
            return (ASTForallConstructStmtNode)getChild(0);
        else
            return null;
    }

    public ASTEndForallStmtNode getEndForallStmt()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.FORALL_CONSTRUCT_631)
            return (ASTEndForallStmtNode)getChild(1);
        else if (getProduction() == Production.FORALL_CONSTRUCT_632)
            return (ASTEndForallStmtNode)getChild(2);
        else
            return null;
    }

    public ASTForallBodyNode getForallBody()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.FORALL_CONSTRUCT_632)
            return (ASTForallBodyNode)getChild(1);
        else
            return null;
    }

    public boolean hasForallBody()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.FORALL_CONSTRUCT_632)
            return getChild(1) != null;
        else
            return false;
    }
}