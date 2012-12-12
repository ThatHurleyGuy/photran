/*******************************************************************************
 * Copyright (c) 2012 TODO COMPANY NAME and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    seanhurley (TODO COMPANY NAME) - Initial API and implementation
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
import org.eclipse.photran.internal.core.parser.ASTFunctionArgNode;
import org.eclipse.photran.internal.core.parser.ASTFunctionReferenceNode;
import org.eclipse.photran.internal.core.parser.ASTFunctionStmtNode;
import org.eclipse.photran.internal.core.parser.ASTInterfaceBodyNode;
import org.eclipse.photran.internal.core.parser.ASTInterfaceStmtNode;
import org.eclipse.photran.internal.core.parser.ASTIntrinsicListNode;
import org.eclipse.photran.internal.core.parser.ASTIntrinsicProcedureNameNode;
import org.eclipse.photran.internal.core.parser.ASTIntrinsicStmtNode;
import org.eclipse.photran.internal.core.parser.ASTSectionSubscriptNode;
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
     * @return An ArrayList of all of the unsafe calls within this node
     */
    public ArrayList<Token> getCallDifference()
    {
        ArrayList<Token> unInterfacedCalls = new ArrayList<Token>();
        for (Token node : callNodes)
        {

            String subroutineName = PhotranVPG.canonicalizeIdentifier(node.getText());
            if (!interfaceNodes.contains(subroutineName))
            {
                System.out.println("Found external call without a matching interface: "
                    + subroutineName);
                unInterfacedCalls.add(node);
            }
            else
            {
                System.out.println("Found matching external call: " + subroutineName);
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
            System.out.println("Found none-intrinsic call node: "
                + node.getSubroutineName().toString());
            callNodes.add(node.getSubroutineName());
        }
        else
        {
            System.out.println("Found Intrinsic call node. Ignoring,");
        }
    }

    @Override
    public void visitASTInterfaceStmtNode(ASTInterfaceStmtNode node) {
        super.visitASTInterfaceStmtNode(node);
        handleASTInterfaceStmtNode(node); }

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
        System.out.println(node.getName());

        ArrayList<Definition> definitions = PhotranVPG.getInstance().findAllModulesNamed(
            node.getName().getText());
        Definition definition = definitions.get(0);
        IFile file = definition.getTokenRef().getFile();
        IFortranAST ast = PhotranVPG.getInstance().acquireTransientAST(file);
        InterfaceFinder finder = new InterfaceFinder(this);
        ast.accept(finder);
    }

    @Override
    public void visitASTNode(IASTNode node) {
        System.out.println(node.getClass());
    }

    @Override
    public void visitASTVarOrFnRefNode(ASTVarOrFnRefNode node) {
        super.visitASTVarOrFnRefNode(node);
        for(Definition def : PhotranVPG.getInstance().findAllExternalSubprogramsNamed(PhotranVPG.canonicalizeIdentifier(node.getName().toString()))) {
            // a) must be a function and b) it is external
            System.out.println("found external function call: " + PhotranVPG.canonicalizeIdentifier(node.getName().getName().getText()));
            callNodes.add(node.getName().getName());
        }
    }

    private void handleASTInterfaceStmtNode(ASTInterfaceStmtNode node) 
    {
        // if the interface has a name that it may be called by, add it
        // to the list of safe call names
        if(node.getGenericName() != null)
            interfaceNodes.add(PhotranVPG.canonicalizeIdentifier(node.getGenericName().toString()));
    }

    private void handleASTInterfaceBodyNode(ASTInterfaceBodyNode node)
    {
        if(node.getSubroutineStmt() != null)
            interfaceNodes.add(PhotranVPG.canonicalizeIdentifier(node.getSubroutineStmt()
                        .getSubroutineName().toString()));
        if(node.getFunctionStmt() != null)
            interfaceNodes.add(PhotranVPG.canonicalizeIdentifier(node.getFunctionStmt().getFunctionName().toString()));
    }

    /**
     * 
     * @author seanhurley
     * 
     *         Used to find all of the interfaces declared in an AST
     */
    private class InterfaceFinder extends ASTVisitor
    {
        private TypesafeCallVisitor visitor;

        public InterfaceFinder(TypesafeCallVisitor visitor)
        {
            this.visitor = visitor;
        }

        @Override
        public void visitASTInterfaceStmtNode(ASTInterfaceStmtNode node)
        {
            visitor.handleASTInterfaceStmtNode(node);
        }

        @Override
        public void visitASTInterfaceBodyNode(ASTInterfaceBodyNode node)
        {
            visitor.handleASTInterfaceBodyNode(node);
        }
    }
}
