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

import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashSet;

import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.photran.core.IFortranAST;
import org.eclipse.photran.internal.core.FortranAST;
import org.eclipse.photran.internal.core.analysis.binding.Definition;
import org.eclipse.photran.internal.core.analysis.binding.ScopingNode;
import org.eclipse.photran.internal.core.lexer.ASTLexerFactory;
import org.eclipse.photran.internal.core.lexer.IAccumulatingLexer;
import org.eclipse.photran.internal.core.parser.ASTEntityDeclNode;
import org.eclipse.photran.internal.core.parser.ASTFunctionParNode;
import org.eclipse.photran.internal.core.parser.ASTFunctionStmtNode;
import org.eclipse.photran.internal.core.parser.ASTMainProgramNode;
import org.eclipse.photran.internal.core.parser.ASTSubroutineParNode;
import org.eclipse.photran.internal.core.parser.ASTSubroutineStmtNode;
import org.eclipse.photran.internal.core.parser.ASTTypeDeclarationStmtNode;
import org.eclipse.photran.internal.core.parser.ASTVisitor;
import org.eclipse.photran.internal.core.parser.IASTListNode;
import org.eclipse.photran.internal.core.parser.IBodyConstruct;
import org.eclipse.photran.internal.core.parser.Parser;
import org.eclipse.photran.internal.core.vpg.PhotranVPG;
import org.eclipse.photran.internal.ui.editor_vpg.Messages;
import org.eclipse.ui.IMarkerResolution;

/**
 * 
 * @author seanhurley
 */
public class TypeSafeCallQuickFixer implements IMarkerResolution
    {
    private String label;

    protected static String EOL = System.getProperty("line.separator"); //$NON-NLS-1$

    public TypeSafeCallQuickFixer(IMarker marker)
    {
        this.label = Messages.FortranEditorTasks_Lint_QuickFixUnsafeCall;
    }

    public String getLabel()
    {
        return label;
    }

    public void run(IMarker marker)
    {
        insertInterface(marker);
        MessageDialog.openInformation(null, "QuickFix Demo",
            "This quick-fix is not yet implemented");
    }

    private void insertInterface(IMarker marker)
    {
        // text name of the call
        String callName = marker.getAttribute(PhotranLint.PHOTRAN_LINT_EXTRA, ""); //$NON-NLS-1$

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

        IASTListNode<IBodyConstruct> body = null;

        try {
            /** @see FortranResourceRefactoring#parseLiteralStatementSequence */
            StringBuilder prog = new StringBuilder();
            prog.append("program p"); //$NON-NLS-1$
            prog.append(EOL);
            prog.append(iface);
            prog.append(EOL);
            prog.append("end program"); //$NON-NLS-1$
            IAccumulatingLexer lexer = new ASTLexerFactory().createLexer(
                    new StringReader(prog.toString()), null, "(none)"); //$NON-NLS-1$
            Parser parser = new Parser();
            FortranAST ast = new FortranAST(null, parser.parse(lexer), lexer.getTokenList());
            body = ((ASTMainProgramNode)ast.getRoot().getProgramUnitList().get(0)).getBody();
        } 
        catch (Exception e)
        {
            throw new Error(e);
        }

        // get the ast associated with the file the marker is on
        IFortranAST ast = PhotranVPG.getInstance().acquireTransientAST(PhotranVPG.getFilenameForIResource(marker.getResource()));
        
        // grab the scoping node and insert our interface there
        ScopingNode node = ast.findFirstTokenOnLine(marker.getAttribute(IMarker.LINE_NUMBER, 0)).getEnclosingScope();
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
