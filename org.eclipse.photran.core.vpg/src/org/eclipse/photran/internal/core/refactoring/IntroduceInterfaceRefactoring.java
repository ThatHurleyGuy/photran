/*******************************************************************************
 * Copyright (c) 2012 University of Illinois at Urbana-Champaign and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Chase Geigle - Initial API and implementation
 *******************************************************************************/
package org.eclipse.photran.internal.core.refactoring;

import java.util.ArrayList;
import java.util.HashSet;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.photran.internal.core.analysis.binding.Definition;
import org.eclipse.photran.internal.core.lexer.Token;
import org.eclipse.photran.internal.core.parser.ASTEntityDeclNode;
import org.eclipse.photran.internal.core.parser.ASTFunctionParNode;
import org.eclipse.photran.internal.core.parser.ASTFunctionStmtNode;
import org.eclipse.photran.internal.core.parser.ASTSubroutineParNode;
import org.eclipse.photran.internal.core.parser.ASTSubroutineStmtNode;
import org.eclipse.photran.internal.core.parser.ASTTypeDeclarationStmtNode;
import org.eclipse.photran.internal.core.parser.ASTVisitor;
import org.eclipse.photran.internal.core.parser.IASTListNode;
import org.eclipse.photran.internal.core.parser.IASTNode;
import org.eclipse.photran.internal.core.parser.IBodyConstruct;
import org.eclipse.photran.internal.core.refactoring.infrastructure.FortranEditorRefactoring;
import org.eclipse.photran.internal.core.reindenter.Reindenter;
import org.eclipse.photran.internal.core.vpg.PhotranVPG;

public class IntroduceInterfaceRefactoring extends FortranEditorRefactoring
{
    @Override
    public String getName()
    {
        return "Introduce Interface Refactoring";
    }

    @SuppressWarnings("unused")
    @Override
    protected void doCheckInitialConditions(RefactoringStatus status, IProgressMonitor pm)
        throws PreconditionFailure
    {
        ensureProjectHasRefactoringEnabled(status);

        Token token = findEnclosingToken(this.astOfFileInEditor, this.selectedRegionInEditor);
        System.out.println(this.selectedRegionInEditor.getStartLine() + " at "+ this.selectedRegionInEditor.getOffset());
        System.out.println("Token null: " + (token == null));
        if (PhotranVPG.getInstance().findAllExternalSubprogramsNamed(token.getText()).isEmpty())
            fail("Select a call to an external subroutine or function.");
    }

    @Override
    protected void doCheckFinalConditions(RefactoringStatus status, IProgressMonitor pm)
        throws PreconditionFailure
    {
        // No final preconditions
    }

    @Override
    protected void doCreateChange(IProgressMonitor pm) throws CoreException,
        OperationCanceledException
    {
        insertInterface();
        // Do something with

        this.addChangeFromModifiedAST(this.fileInEditor, pm);

        vpg.releaseAST(this.fileInEditor);
    }

    private void insertInterface()
    {
        Token token = findEnclosingToken(this.astOfFileInEditor, this.selectedRegionInEditor);
        String callName = token.getText();
        
        String iface = null;
        ArrayList<Definition> defs = PhotranVPG.getInstance().findAllExternalSubprogramsNamed(callName);
        for (Definition def : defs) {
            ASTFunctionStmtNode function = def.getTokenRef().getASTNode().findNearestAncestor(ASTFunctionStmtNode.class);
            if (function != null) {
                iface = generateFunctionInterface(function);
            } else {
                ASTSubroutineStmtNode subroutine = def.getTokenRef().getASTNode().findNearestAncestor(ASTSubroutineStmtNode.class);
                iface = generateSubroutineInterface(subroutine);
            }
        }
        System.out.println(iface);

        IASTListNode<IBodyConstruct> intrNode = parseLiteralStatementSequence(iface);

        @SuppressWarnings("unchecked")
        IASTListNode<IASTNode> body = (IASTListNode<IASTNode>)token.getEnclosingScope().getOrCreateBody();
        body.addAll(findIndexToInsertStatement(body), intrNode);
        Reindenter.reindent(intrNode, this.astOfFileInEditor, Reindenter.Strategy.REINDENT_EACH_LINE);
    }

    private String generateFunctionInterface(ASTFunctionStmtNode function)
    {
        HashSet<String> neededDeclarations = new HashSet<String>();
        ArrayList<String> parameters = new ArrayList<String>();
        String result = null;

        // if we have a result clause, we need the declaration of it in the
        // interface
        if (function.hasResultClause()) {
            for (Definition def : function.getName().resolveBinding()) {
                neededDeclarations.add(def.getCanonicalizedName());
                result = " result(" + def.getCanonicalizedName() + ")"; //$NON-NLS-1$ //$NON-NLS-2$
            }
        }

        // we also need the declaration of all function parameters
        for (ASTFunctionParNode param : function.getFunctionPars()) {
            for (Definition def : param.getVariableName().resolveBinding()) {
                neededDeclarations.add(def.getCanonicalizedName());
                parameters.add(def.getCanonicalizedName());
            }
        }

        DeclarationVisitor visitor = new DeclarationVisitor(neededDeclarations);
        function.getParent().accept(visitor);

        StringBuilder builder = new StringBuilder();
        builder.append("interface"); //$NON-NLS-1$
        builder.append(EOL);

        // begin the function and parameter list
        builder.append("function "); //$NON-NLS-1$
        builder.append(function.getFunctionName().getFunctionName().getText());
        builder.append(makeParameterList(parameters));
        
        if (function.hasResultClause())
            builder.append(result);
        builder.append(EOL);

        builder.append(visitor.getDeclarations());
        builder.append(EOL);
        builder.append("end function"); //$NON-NLS-1$
        builder.append(EOL);
        builder.append("end interface"); //$NON-NLS-1$
        return builder.toString();
    }

    private String generateSubroutineInterface(ASTSubroutineStmtNode subroutine)
    {
        HashSet<String> neededDeclarations = new HashSet<String>();
        ArrayList<String> parameters = new ArrayList<String>();

        // we also need the declaration of all function parameters
        for (ASTSubroutineParNode param : subroutine.getSubroutinePars()) {
            for (Definition def : param.getVariableName().resolveBinding()) {
                neededDeclarations.add(def.getCanonicalizedName());
                parameters.add(def.getCanonicalizedName());
            }
        }

        DeclarationVisitor visitor = new DeclarationVisitor(neededDeclarations);
        subroutine.getParent().accept(visitor);

        StringBuilder builder = new StringBuilder();
        builder.append("interface"); //$NON-NLS-1$
        builder.append(EOL);

        // begin the subroutine and parameter list
        builder.append("subroutine  "); //$NON-NLS-1$
        builder.append(subroutine.getSubroutineName().getSubroutineName().getText());
        builder.append(makeParameterList(parameters));
        builder.append(EOL);

        builder.append(visitor.getDeclarations());
        builder.append(EOL);
        builder.append("end subroutine"); //$NON-NLS-1$
        builder.append(EOL);
        builder.append("end interface"); //$NON-NLS-1$
        return builder.toString();
    }

    private String makeParameterList(ArrayList<String> parameters)
    {
        StringBuilder builder = new StringBuilder();
        builder.append("("); //$NON-NLS-1$
        for (int i = 0; i < parameters.size(); ++i ) {
            if (i > 0) builder.append(","); //$NON-NLS-1$
            builder.append(parameters.get(i));
        }
        builder.append(")"); //$NON-NLS-1$
        return builder.toString();
    }

    private class DeclarationVisitor extends ASTVisitor {
        private HashSet<String> vars;
        private StringBuilder declarations;

        public DeclarationVisitor(HashSet<String> vars) {
            this.vars = vars;
            this.declarations = new StringBuilder();
        }

        public String getDeclarations() {
            return declarations.toString();
        }

        @Override
        public void visitASTTypeDeclarationStmtNode(ASTTypeDeclarationStmtNode node)
        {
            super.visitASTTypeDeclarationStmtNode(node);
            for (ASTEntityDeclNode entityDecl : node.getEntityDeclList()) {
                for (Definition def : entityDecl.getObjectName().getObjectName().resolveBinding()) {
                    if (vars.contains(def.getCanonicalizedName())) {
                        declarations.append(node.toString());
                        return;
                    }
                }
            }
        }
    }
}
