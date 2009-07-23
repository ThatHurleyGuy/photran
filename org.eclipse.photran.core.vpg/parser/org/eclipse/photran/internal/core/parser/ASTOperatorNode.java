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

import org.eclipse.photran.internal.core.lexer.*;                   import org.eclipse.photran.internal.core.analysis.binding.ScopingNode;                   import org.eclipse.photran.internal.core.SyntaxException;                   import java.io.IOException;

public class ASTOperatorNode extends ASTNode implements IDefinedOperator
{
    org.eclipse.photran.internal.core.lexer.Token hasAndOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token hasNeOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token definedUnaryOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token hasOrOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token hasLtOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token hasConcatOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token hasTimesOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token hasNeqvOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token hasGeOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token hasGtOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token hasEqvOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token hasLeOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token hasEqEqOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token hasMinusOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token hasSlashEqOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token hasDivideOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token customDefinedOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token hasEqOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token hasPlusOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token hasPowerOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token hasNotOp; // in ASTOperatorNode
    org.eclipse.photran.internal.core.lexer.Token definedBinaryOp; // in ASTOperatorNode

    public boolean hasAndOp()
    {
        return this.hasAndOp != null;
    }

    public void setHasAndOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasAndOp = newValue;
    }


    public boolean hasNeOp()
    {
        return this.hasNeOp != null;
    }

    public void setHasNeOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasNeOp = newValue;
    }


    public org.eclipse.photran.internal.core.lexer.Token getDefinedUnaryOp()
    {
        return this.definedUnaryOp;
    }

    public void setDefinedUnaryOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.definedUnaryOp = newValue;
    }


    public boolean hasOrOp()
    {
        return this.hasOrOp != null;
    }

    public void setHasOrOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasOrOp = newValue;
    }


    public boolean hasLtOp()
    {
        return this.hasLtOp != null;
    }

    public void setHasLtOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasLtOp = newValue;
    }


    public boolean hasConcatOp()
    {
        return this.hasConcatOp != null;
    }

    public void setHasConcatOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasConcatOp = newValue;
    }


    public boolean hasTimesOp()
    {
        return this.hasTimesOp != null;
    }

    public void setHasTimesOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasTimesOp = newValue;
    }


    public boolean hasNeqvOp()
    {
        return this.hasNeqvOp != null;
    }

    public void setHasNeqvOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasNeqvOp = newValue;
    }


    public boolean hasGeOp()
    {
        return this.hasGeOp != null;
    }

    public void setHasGeOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasGeOp = newValue;
    }


    public boolean hasGtOp()
    {
        return this.hasGtOp != null;
    }

    public void setHasGtOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasGtOp = newValue;
    }


    public boolean hasEqvOp()
    {
        return this.hasEqvOp != null;
    }

    public void setHasEqvOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasEqvOp = newValue;
    }


    public boolean hasLeOp()
    {
        return this.hasLeOp != null;
    }

    public void setHasLeOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasLeOp = newValue;
    }


    public boolean hasEqEqOp()
    {
        return this.hasEqEqOp != null;
    }

    public void setHasEqEqOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasEqEqOp = newValue;
    }


    public boolean hasMinusOp()
    {
        return this.hasMinusOp != null;
    }

    public void setHasMinusOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasMinusOp = newValue;
    }


    public boolean hasSlashEqOp()
    {
        return this.hasSlashEqOp != null;
    }

    public void setHasSlashEqOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasSlashEqOp = newValue;
    }


    public boolean hasDivideOp()
    {
        return this.hasDivideOp != null;
    }

    public void setHasDivideOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasDivideOp = newValue;
    }


    public org.eclipse.photran.internal.core.lexer.Token getCustomDefinedOp()
    {
        return this.customDefinedOp;
    }

    public void setCustomDefinedOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.customDefinedOp = newValue;
    }


    public boolean hasEqOp()
    {
        return this.hasEqOp != null;
    }

    public void setHasEqOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasEqOp = newValue;
    }


    public boolean hasPlusOp()
    {
        return this.hasPlusOp != null;
    }

    public void setHasPlusOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasPlusOp = newValue;
    }


    public boolean hasPowerOp()
    {
        return this.hasPowerOp != null;
    }

    public void setHasPowerOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasPowerOp = newValue;
    }


    public boolean hasNotOp()
    {
        return this.hasNotOp != null;
    }

    public void setHasNotOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.hasNotOp = newValue;
    }


    public org.eclipse.photran.internal.core.lexer.Token getDefinedBinaryOp()
    {
        return this.definedBinaryOp;
    }

    public void setDefinedBinaryOp(org.eclipse.photran.internal.core.lexer.Token newValue)
    {
        this.definedBinaryOp = newValue;
    }


    public void accept(IASTVisitor visitor)
    {
        visitor.visitASTOperatorNode(this);
        visitor.visitIDefinedOperator(this);
        visitor.visitASTNode(this);
    }

    @Override protected int getNumASTFields()
    {
        return 22;
    }

    @Override protected IASTNode getASTField(int index)
    {
        switch (index)
        {
        case 0:  return this.hasAndOp;
        case 1:  return this.hasNeOp;
        case 2:  return this.definedUnaryOp;
        case 3:  return this.hasOrOp;
        case 4:  return this.hasLtOp;
        case 5:  return this.hasConcatOp;
        case 6:  return this.hasTimesOp;
        case 7:  return this.hasNeqvOp;
        case 8:  return this.hasGeOp;
        case 9:  return this.hasGtOp;
        case 10: return this.hasEqvOp;
        case 11: return this.hasLeOp;
        case 12: return this.hasEqEqOp;
        case 13: return this.hasMinusOp;
        case 14: return this.hasSlashEqOp;
        case 15: return this.hasDivideOp;
        case 16: return this.customDefinedOp;
        case 17: return this.hasEqOp;
        case 18: return this.hasPlusOp;
        case 19: return this.hasPowerOp;
        case 20: return this.hasNotOp;
        case 21: return this.definedBinaryOp;
        default: return null;
        }
    }

    @Override protected void setASTField(int index, IASTNode value)
    {
        switch (index)
        {
        case 0:  this.hasAndOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 1:  this.hasNeOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 2:  this.definedUnaryOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 3:  this.hasOrOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 4:  this.hasLtOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 5:  this.hasConcatOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 6:  this.hasTimesOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 7:  this.hasNeqvOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 8:  this.hasGeOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 9:  this.hasGtOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 10: this.hasEqvOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 11: this.hasLeOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 12: this.hasEqEqOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 13: this.hasMinusOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 14: this.hasSlashEqOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 15: this.hasDivideOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 16: this.customDefinedOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 17: this.hasEqOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 18: this.hasPlusOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 19: this.hasPowerOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 20: this.hasNotOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        case 21: this.definedBinaryOp = (org.eclipse.photran.internal.core.lexer.Token)value; return;
        default: throw new IllegalArgumentException("Invalid index");
        }
    }
}

