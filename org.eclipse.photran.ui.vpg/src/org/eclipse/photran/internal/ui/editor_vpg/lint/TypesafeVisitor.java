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
import org.eclipse.photran.internal.core.parser.ASTCallStmtNode;
import org.eclipse.photran.internal.core.parser.ASTInterfaceBodyNode;
import org.eclipse.photran.internal.core.parser.ASTUseStmtNode;
import org.eclipse.photran.internal.core.parser.ASTVisitor;
import org.eclipse.photran.internal.core.parser.IASTNode;
import org.eclipse.photran.internal.core.vpg.PhotranVPG;

/**
 * 
 * @author seanhurley
 */

public class TypesafeVisitor extends ASTVisitor
{
    private Set<ASTCallStmtNode> callNodes;

    private HashSet<String> interfaceNodes;

    /**
     * @param node The IASTNode that should be searched for unsafe calls
     * @return An ArrayList of all of the unsafe calls within this node
     */
    public static ArrayList<ASTCallStmtNode> getUnsafeCalls(IASTNode node) {
        TypesafeVisitor visitor = new TypesafeVisitor();
        node.accept(visitor);
        return visitor.getCallDifference();
    }
    
    public TypesafeVisitor()
    {
        callNodes = new HashSet<ASTCallStmtNode>();
        interfaceNodes = new HashSet<String>();
    }

    /**
     * @return An ArrayList of all of the unsafe calls within this node
     */
    public ArrayList<ASTCallStmtNode> getCallDifference()
    {
        ArrayList<ASTCallStmtNode> unInterfacedCalls = new ArrayList<ASTCallStmtNode>();
        for (ASTCallStmtNode node : callNodes)
        {

            String subroutineName = PhotranVPG.canonicalizeIdentifier(node.getSubroutineName()
                .getText());
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
        System.out.println("Found call node: " + node.getSubroutineName().toString());
        callNodes.add(node);
    }

    @Override
    public void visitASTInterfaceBodyNode(ASTInterfaceBodyNode node)
    {
        super.visitASTInterfaceBodyNode(node);
        interfaceNodes.add(PhotranVPG.canonicalizeIdentifier(node.getSubroutineStmt()
            .getSubroutineName().toString()));
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
        InterfaceFinder finder = new InterfaceFinder();
        ast.accept(finder);

        interfaceNodes.addAll(finder.getInterfaceNodes());
    }

    /**
     * 
     * @author seanhurley
     * 
     * Used to find all of the interfaces declared in an AST
     */
    private class InterfaceFinder extends ASTVisitor
    {
        private HashSet<String> interfaceNodes;

        public InterfaceFinder()
        {
            this.interfaceNodes = new HashSet<String>();
        }

        /**
         * @return the interfaceNodes
         */
        public HashSet<String> getInterfaceNodes()
        {
            return interfaceNodes;
        }

        @Override
        public void visitASTInterfaceBodyNode(ASTInterfaceBodyNode node)
        {
            super.visitASTInterfaceBodyNode(node);
            interfaceNodes.add(PhotranVPG.canonicalizeIdentifier(node.getSubroutineStmt()
                .getSubroutineName().toString()));
        }
    }
}
