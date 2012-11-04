/*******************************************************************************
 * Copyright (c) 2012 UIUC and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Sean Hurley (UIUC) - Initial API and implementation
 *    Chase Geigle (UIUC) - External Function handling
 *******************************************************************************/
package org.eclipse.photran.internal.ui.editor_vpg.lint;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.photran.core.IFortranAST;
import org.eclipse.photran.internal.core.analysis.binding.Definition;
import org.eclipse.photran.internal.core.analysis.binding.Intrinsic;
import org.eclipse.photran.internal.core.lexer.Token;
import org.eclipse.photran.internal.core.parser.ASTCallStmtNode;
import org.eclipse.photran.internal.core.parser.ASTFunctionStmtNode;
import org.eclipse.photran.internal.core.parser.ASTInterfaceBodyNode;
import org.eclipse.photran.internal.core.parser.ASTInterfaceStmtNode;
import org.eclipse.photran.internal.core.parser.ASTSubroutineStmtNode;
import org.eclipse.photran.internal.core.parser.ASTUseStmtNode;
import org.eclipse.photran.internal.core.parser.ASTVarOrFnRefNode;
import org.eclipse.photran.internal.core.parser.ASTVisitor;
import org.eclipse.photran.internal.core.parser.IASTNode;
import org.eclipse.photran.internal.core.vpg.PhotranVPG;

/**
 * 
 * @author seanhurley
 */

public class TypesafeCallVisitor extends ASTVisitor
{
    private Set<Token> callNodes;

    private HashSet<String> interfaceNodes;

    /**
     * A way to statically get a list of unsafe calls from the IASTNode
     * 
     * @param node The IASTNode that should be searched for unsafe calls
     * @return An ArrayList of all of the unsafe calls within this node
     */
    public static ArrayList<Token> getUnsafeCalls(IASTNode node)
    {
        TypesafeCallVisitor visitor = new TypesafeCallVisitor();
        node.accept(visitor);
        return visitor.getCallDifference();
    }

    public TypesafeCallVisitor()
    {
        callNodes = new HashSet<Token>();
        interfaceNodes = new HashSet<String>();
    }

    /**
     * Will calculate the difference between the list of found interfaces and the actual
     * functions/subroutines
     * 
     * @return An ArrayList of all of the unsafe calls within this node
     */
    private ArrayList<Token> getCallDifference()
    {
        ArrayList<Token> unInterfacedCalls = new ArrayList<Token>();
        for (Token node : callNodes)
        {

            String subroutineName = PhotranVPG.canonicalizeIdentifier(node.getText());
            if (!interfaceNodes.contains(subroutineName))
            {
                unInterfacedCalls.add(node);
            }
        }

        return unInterfacedCalls;
    }

    @Override
    public void visitASTCallStmtNode(ASTCallStmtNode node)
    {
        super.visitASTCallStmtNode(node);
        Definition test = Intrinsic.resolve(node.getSubroutineName());
        if (test == null)
        {
            callNodes.add(node.getSubroutineName());
        }
    }

    @Override
    public void visitASTInterfaceStmtNode(ASTInterfaceStmtNode node)
    {
        super.visitASTInterfaceStmtNode(node);
        handleASTInterfaceStmtNode(node);
    }

    @Override
    public void visitASTInterfaceBodyNode(ASTInterfaceBodyNode node)
    {
        super.visitASTInterfaceBodyNode(node);
        handleASTInterfaceBodyNode(node);
    }

    @Override
    public void visitASTUseStmtNode(ASTUseStmtNode node)
    {
        super.visitASTUseStmtNode(node);

        ArrayList<Definition> definitions = PhotranVPG.getInstance().findAllModulesNamed(
            node.getName().getText());
        Definition definition = definitions.get(0);
        IFile file = definition.getTokenRef().getFile();
        IFortranAST ast = PhotranVPG.getInstance().acquireTransientAST(file);
        InterfaceSubroutineFinder finder = new InterfaceSubroutineFinder(this);
        ast.accept(finder);
    }

    @SuppressWarnings("unused")
    @Override
    public void visitASTVarOrFnRefNode(ASTVarOrFnRefNode node)
    {
        super.visitASTVarOrFnRefNode(node);
        for (Definition def : PhotranVPG.getInstance().findAllExternalSubprogramsNamed(
            PhotranVPG.canonicalizeIdentifier(node.getName().toString())))
        {
            // a) must be a function and b) it is external
            callNodes.add(node.getName().getName());
        }
    }

    private void handleASTInterfaceStmtNode(ASTInterfaceStmtNode node)
    {
        // if the interface has a name that it may be called by, add it
        // to the list of safe call names
        if (node.getGenericName() != null)
            interfaceNodes.add(PhotranVPG.canonicalizeIdentifier(node.getGenericName().toString()));
    }

    private void handleASTInterfaceBodyNode(ASTInterfaceBodyNode node)
    {
        if (node.getSubroutineStmt() != null)
            interfaceNodes.add(PhotranVPG.canonicalizeIdentifier(node.getSubroutineStmt()
                .getSubroutineName().toString()));
        if (node.getFunctionStmt() != null)
            interfaceNodes.add(PhotranVPG.canonicalizeIdentifier(node.getFunctionStmt()
                .getFunctionName().toString()));
    }

    private void handleASTSubroutineSTMTNode(ASTSubroutineStmtNode node)
    {
        interfaceNodes.add(PhotranVPG.canonicalizeIdentifier(node.getSubroutineName()
            .getSubroutineName().getText()));
    }

    private void handleASTFunctionSTMTNode(ASTFunctionStmtNode node)
    {
        interfaceNodes.add(PhotranVPG.canonicalizeIdentifier(node.getFunctionName()
            .getFunctionName().getText()));
    }

    /**
     * 
     * @author seanhurley
     * 
     *         Used to find all of the interfaces declared in an AST. This will generally be called
     *         on when we see a "USE" statement in the code and we only care about the interfaces
     *         within its module
     */
    private class InterfaceSubroutineFinder extends ASTVisitor
    {
        private TypesafeCallVisitor visitor;

        public InterfaceSubroutineFinder(TypesafeCallVisitor visitor)
        {
            this.visitor = visitor;
        }

        @Override
        public void visitASTFunctionStmtNode(ASTFunctionStmtNode node)
        {
            super.visitASTFunctionStmtNode(node);
            visitor.handleASTFunctionSTMTNode(node);
        }

        @Override
        public void visitASTSubroutineStmtNode(ASTSubroutineStmtNode node)
        {
            super.visitASTSubroutineStmtNode(node);
            visitor.handleASTSubroutineSTMTNode(node);
        }

        @Override
        public void visitASTInterfaceStmtNode(ASTInterfaceStmtNode node)
        {
            super.visitASTInterfaceStmtNode(node);
            visitor.handleASTInterfaceStmtNode(node);
        }

        @Override
        public void visitASTInterfaceBodyNode(ASTInterfaceBodyNode node)
        {
            super.visitASTInterfaceBodyNode(node);
            visitor.handleASTInterfaceBodyNode(node);
        }
    }
}
