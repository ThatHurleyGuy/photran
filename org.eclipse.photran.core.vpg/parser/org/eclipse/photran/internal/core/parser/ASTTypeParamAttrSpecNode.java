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

import java.io.PrintStream;
import java.util.Iterator;

import java.util.List;

import org.eclipse.photran.internal.core.parser.Parser.ASTNode;
import org.eclipse.photran.internal.core.parser.Parser.ASTNodeWithErrorRecoverySymbols;
import org.eclipse.photran.internal.core.parser.Parser.IASTListNode;
import org.eclipse.photran.internal.core.parser.Parser.IASTNode;
import org.eclipse.photran.internal.core.parser.Parser.IASTVisitor;
import org.eclipse.photran.internal.core.lexer.Token;

import org.eclipse.photran.internal.core.lexer.*;                   import org.eclipse.photran.internal.core.analysis.binding.ScopingNode;

public class ASTTypeParamAttrSpecNode extends ASTNode
{
    org.eclipse.photran.internal.core.lexer.Token isKind; // in ASTTypeParamAttrSpecNode
    org.eclipse.photran.internal.core.lexer.Token isLen; // in ASTTypeParamAttrSpecNode

    public boolean isKind()
    {
        return this.isKind != null;
    }

    public void setIsKind(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.isKind = newValue;
    }


    public boolean isLen()
    {
        return this.isLen != null;
    }

    public void setIsLen(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.isLen = newValue;
    }


    public void accept(IASTVisitor visitor)
    {
        visitor.visitASTTypeParamAttrSpecNode(this);
        visitor.visitASTNode(this);
    }

    @Override protected int getNumASTFields()
    {
        return 2;
    }

    @Override protected IASTNode getASTField(int index)
    {
        switch (index)
        {
        case 0:  return this.isKind;
        case 1:  return this.isLen;
        default: return null;
        }
    }

    @Override protected void setASTField(int index, IASTNode value)
    {
        switch (index)
        {
        case 0:  this.isKind = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 1:  this.isLen = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        default: throw new IllegalArgumentException("Invalid index");
        }
    }
}

