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

import org.eclipse.photran.internal.ui.editor_vpg.Messages;
import org.eclipse.core.resources.IMarker;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.photran.internal.core.analysis.binding.Definition;
import org.eclipse.photran.internal.core.parser.ASTFunctionParNode;
import org.eclipse.photran.internal.core.parser.ASTFunctionStmtNode;
import org.eclipse.photran.internal.core.parser.ASTSubroutineParNode;
import org.eclipse.photran.internal.core.parser.ASTSubroutineStmtNode;
import org.eclipse.photran.internal.core.vpg.PhotranVPG;
import org.eclipse.ui.IMarkerResolution;

/**
 * 
 * @author seanhurley
 */
public class TypeSafeCallQuickFixer implements IMarkerResolution
    {
    private String label;

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
        System.out.println("---QuickFix---");
        // text name of the call
        String callName = marker.getAttribute(PhotranLint.PHOTRAN_LINT_EXTRA, "");
        /*
        IFortranAST ast = PhotranVPG.getInstance().acquireTransientAST((IFile)marker.getResource());

        */
        /*
        DefinitionMap<Definition> defMap = new DefinitionMap<Definition>(ast)
        {
            // why??
            @Override
            protected Definition map(String qualifiedName, Definition def)
            {
                return def;
            }
        };
        */

        ArrayList<Definition> parameterDefinitions = new ArrayList<Definition>();
        ArrayList<Definition> defs = PhotranVPG.getInstance().findAllExternalSubprogramsNamed(callName);
        ASTFunctionStmtNode function = null;
        ASTSubroutineStmtNode subroutine = null;
        for (Definition def : defs) {
            function = def.getTokenRef().getASTNode().findNearestAncestor(ASTFunctionStmtNode.class);
            subroutine = def.getTokenRef().getASTNode().findNearestAncestor(ASTSubroutineStmtNode.class);
            if (function != null) {
                for (ASTFunctionParNode param : function.getFunctionPars()) {
                    parameterDefinitions.addAll(param.getVariableName().resolveBinding());
                }
            } else if (subroutine != null) {
                for (ASTSubroutineParNode param : subroutine.getSubroutinePars()) {
                    parameterDefinitions.addAll(param.getVariableName().resolveBinding());
                }
            }
        }

        StringBuilder sb = new StringBuilder();
        sb.append("interface\n"); //$NON-NLS-1$
        if (subroutine == null) {
            sb.append("function "); //$NON-NLS-1$
        } else {
            sb.append("subroutine "); //$NON-NLS-1$
        }
        sb.append(callName);
        sb.append("("); //$NON-NLS-1$
        for (int i = 0; i < parameterDefinitions.size(); ++i) {
            sb.append(parameterDefinitions.get(i).getCanonicalizedName());
            if (i != parameterDefinitions.size() - 1)
                sb.append(", "); //$NON-NLS-1$
        }
        sb.append(")\n"); //$NON-NLS-1$
        for (Definition def : parameterDefinitions) {
            sb.append(def.getType().toString());
            if (def.isIntentIn()) {
                if (def.isIntentOut()) {
                    sb.append(", intent(INOUT)"); //$NON-NLS-1$
                } else {
                    sb.append(", intent(IN)"); //$NON-NLS-1$
                }
            } else if (def.isIntentOut()) {
                sb.append(", intent(OUT)"); //$NON-NLS-1$
            }
            sb.append(" :: "); //$NON-NLS-1$
            sb.append(def.getCanonicalizedName());
            sb.append("\n"); //$NON-NLS-1$
        }
        if (subroutine == null) {
            sb.append("end function\n"); //$NON-NLS-1$
        } else {
            sb.append("end subroutine\n"); //$NON-NLS-1$
        }
        sb.append("end interface\n"); //$NON-NLS-1$
        System.out.println(sb.toString());
    }
}
