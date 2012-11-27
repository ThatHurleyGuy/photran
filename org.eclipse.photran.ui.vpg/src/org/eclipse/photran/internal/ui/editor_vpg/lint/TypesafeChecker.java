/*******************************************************************************
 * Copyright (c) 2012 TODO COMPANY NAME and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Sean Hurley (TODO COMPANY NAME) - Initial API and implementation
 *    Chase Geigle (TODO COMPANY NAME) - Adding IMarkers
 *******************************************************************************/
package org.eclipse.photran.internal.ui.editor_vpg.lint;

import java.util.ArrayList;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.photran.internal.core.analysis.binding.Definition;
import org.eclipse.photran.internal.core.lexer.Token;
import org.eclipse.photran.internal.core.lexer.TokenList;
import org.eclipse.photran.internal.core.parser.ASTCallStmtNode;
import org.eclipse.photran.internal.core.parser.ASTExecutableProgramNode;
import org.eclipse.photran.internal.core.parser.ASTListNode;
import org.eclipse.photran.internal.core.parser.IASTNode;
import org.eclipse.photran.internal.ui.editor.FortranEditor;
import org.eclipse.photran.internal.ui.editor_vpg.DefinitionMap;
import org.eclipse.photran.internal.ui.editor_vpg.FortranEditorTasks;
import org.eclipse.photran.internal.ui.editor_vpg.IFortranEditorASTTask;

/**
 *
 * @author Sean Hurley, Chase Geigle
 */
public class TypesafeChecker implements IFortranEditorASTTask
{
    private ArrayList<IMarker> markers;

    public void setup(FortranEditor editor)
    {
        this.markers = new ArrayList<IMarker>();
        FortranEditorTasks.instance(editor).addASTTask(this);
    }

    @Override
    public boolean handle(ASTExecutableProgramNode ast, TokenList tokenList,
        DefinitionMap<Definition> defMap)
    {    
        System.out.println("---New Scan---");
        ArrayList<ASTCallStmtNode> unsafeCalls = new ArrayList<ASTCallStmtNode>();
        //Iterable< ? extends IASTNode> rootNodes = ast.getRoot().getChildren();
        Iterable< ? extends IASTNode> rootNodes = ast.getChildren();
        for (IASTNode rootNode : rootNodes)
        {
            ASTListNode<IASTNode> nodeList = (ASTListNode<IASTNode>)rootNode;
            for (IASTNode node : nodeList)
            {
                unsafeCalls.addAll(TypesafeVisitor.getUnsafeCalls(node));
            }
        }

        // remove existing markers created from type safe call scans
        for (IMarker marker : markers ) 
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
        for (ASTCallStmtNode unsafe : unsafeCalls)
        {
            try
            {
                Token token = unsafe.getSubroutineName();
                IMarker marker = token.getPhysicalFile().getIFile().createMarker(IMarker.PROBLEM);
                marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_WARNING);
                marker.setAttribute(IMarker.CHAR_START, token.getFileOffset());
                marker.setAttribute(IMarker.CHAR_END, token.getFileOffset()+token.getLength());
                marker.setAttribute(IMarker.LINE_NUMBER, token.getLine());
                marker.setAttribute(IMarker.MESSAGE, "Call to external subroutine is not type safe, consider adding an interface"); //$NON-NLS-1$
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
