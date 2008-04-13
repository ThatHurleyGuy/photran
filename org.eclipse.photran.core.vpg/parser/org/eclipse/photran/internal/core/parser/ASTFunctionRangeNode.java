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

class ASTFunctionRangeNode extends InteriorNode
{
    ASTFunctionRangeNode(Production production, List<CSTNode> childNodes, List<CSTNode> discardedSymbols)
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

    public ASTBodyNode getBody()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.FUNCTION_RANGE_18)
            return (ASTBodyNode)getChild(0);
        else if (getProduction() == Production.FUNCTION_RANGE_20)
            return (ASTBodyNode)((ASTBodyPlusInternalsNode)getChild(0)).getBody();
        else
            return null;
    }

    public ASTEndFunctionStmtNode getEndFunctionStmt()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.FUNCTION_RANGE_18)
            return (ASTEndFunctionStmtNode)getChild(1);
        else if (getProduction() == Production.FUNCTION_RANGE_19)
            return (ASTEndFunctionStmtNode)getChild(0);
        else if (getProduction() == Production.FUNCTION_RANGE_20)
            return (ASTEndFunctionStmtNode)getChild(1);
        else
            return null;
    }

    public ASTContainsStmtNode getContainsStmt()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.FUNCTION_RANGE_20)
            return (ASTContainsStmtNode)((ASTBodyPlusInternalsNode)getChild(0)).getContainsStmt();
        else
            return null;
    }

    public ASTInternalSubprogramsNode getInternalSubprograms()
    {
        if (treeHasBeenModified()) throw new IllegalStateException("Accessor methods cannot be called on the nodes of a CST after it has been modified");

        if (getProduction() == Production.FUNCTION_RANGE_20)
            return (ASTInternalSubprogramsNode)((ASTBodyPlusInternalsNode)getChild(0)).getInternalSubprograms();
        else
            return null;
    }

    @Override protected boolean childIsPulledUp(int index)
    {
        if (getProduction() == Production.FUNCTION_RANGE_20 && index == 0)
            return true;
        else
            return false;
    }
}