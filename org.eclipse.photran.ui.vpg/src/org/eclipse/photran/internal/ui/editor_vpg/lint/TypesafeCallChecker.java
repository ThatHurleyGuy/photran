/*******************************************************************************
 * Copyright (c) 2012 UIUC and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Sean Hurley (UIUC) - Initial API and implementation
 *    Chase Geigle (UIUC) - Adding IMarkers
 *******************************************************************************/
package org.eclipse.photran.internal.ui.editor_vpg.lint;

import java.util.ArrayList;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.photran.internal.core.analysis.binding.Definition;
import org.eclipse.photran.internal.core.lexer.Token;
import org.eclipse.photran.internal.core.lexer.TokenList;
import org.eclipse.photran.internal.core.parser.ASTExecutableProgramNode;
import org.eclipse.photran.internal.core.parser.ASTListNode;
import org.eclipse.photran.internal.core.parser.IASTNode;
import org.eclipse.photran.internal.ui.editor.FortranEditor;
import org.eclipse.photran.internal.ui.editor_vpg.DefinitionMap;
import org.eclipse.photran.internal.ui.editor_vpg.FortranEditorTasks;
import org.eclipse.photran.internal.ui.editor_vpg.IFortranEditorASTTask;
import org.eclipse.photran.internal.ui.editor_vpg.Messages;

/**
 * 
 * @author Sean Hurley, Chase Geigle
 */
public class TypesafeCallChecker implements IFortranEditorASTTask
{
    private ArrayList<IMarker> markers;

    public void setup(FortranEditor editor)
    {
        this.markers = new ArrayList<IMarker>();
        FortranEditorTasks.instance(editor).addASTTask(this);
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean handle(ASTExecutableProgramNode ast, TokenList tokenList,
        DefinitionMap<Definition> defMap)
    {
        if (defMap == null) return true;

        ArrayList<Token> unsafeCalls = new ArrayList<Token>();
        Iterable< ? extends IASTNode> rootNodes = ast.getChildren();
        for (IASTNode rootNode : rootNodes)
        {
            ASTListNode<IASTNode> nodeList = (ASTListNode<IASTNode>)rootNode;
            for (IASTNode node : nodeList)
            {
                unsafeCalls.addAll(TypesafeCallVisitor.getUnsafeCalls(node));
            }
        }

        try
        {
            IMarker[] markers = ast.findFirstToken().getPhysicalFile().getIFile()
                .findMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);

            for (IMarker marker : markers)
            {
                Integer attribute = marker.getAttribute(PhotranLint.PHOTRAN_LINT_ATTRIBUTE, -1);
                if (attribute == PhotranLint.UNSAFE_CALL_VALUE)
                {
                    marker.delete();
                }
            }
        }
        catch (CoreException e1)
        {
            e1.printStackTrace();
        }

        // remove existing markers created from type safe call scans
        for (IMarker marker : markers)
        {
            try
            {
                marker.delete();
            }
            catch (CoreException e)
            {
                e.printStackTrace();
            }
        }
        markers.clear();

        // mark all unsafe calls with IMarkers
        for (Token token : unsafeCalls)
        {
            try
            {
                // Here we create a marker with the type given since the eclipse api will then use
                // that type to properly style it and place Annotations and QuickFixes on it
                IMarker marker = token.getPhysicalFile().getIFile()
                    .createMarker("org.eclipse.photran.internal.uiuc.editor_vpg.lint.marker"); //$NON-NLS-1$
                marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_WARNING);
                marker.setAttribute(IMarker.CHAR_START, token.getFileOffset());
                marker.setAttribute(IMarker.CHAR_END, token.getFileOffset() + token.getLength());
                marker.setAttribute(PhotranLint.PHOTRAN_LINT_ATTRIBUTE,
                    PhotranLint.UNSAFE_CALL_VALUE);
                marker.setAttribute(PhotranLint.PHOTRAN_LINT_EXTRA, token.getText());
                marker.setAttribute(IMarker.MESSAGE,
                    Messages.FortranEditorTasks_Lint_CallToExternalSubroutineUnsafe);
                markers.add(marker);
            }
            catch (CoreException e)
            {
                e.printStackTrace();
            }
        }
        return true;
    }
}
