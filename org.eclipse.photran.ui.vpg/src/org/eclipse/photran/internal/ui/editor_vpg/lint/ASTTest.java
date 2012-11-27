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
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.photran.core.IFortranAST;
import org.eclipse.photran.internal.core.analysis.binding.Definition;
import org.eclipse.photran.internal.core.lexer.TokenList;
import org.eclipse.photran.internal.core.parser.ASTCallStmtNode;
import org.eclipse.photran.internal.core.parser.ASTExecutableProgramNode;
import org.eclipse.photran.internal.core.parser.ASTInterfaceBodyNode;
import org.eclipse.photran.internal.core.parser.ASTListNode;
import org.eclipse.photran.internal.core.parser.ASTUseStmtNode;
import org.eclipse.photran.internal.core.parser.ASTVisitor;
import org.eclipse.photran.internal.core.parser.IASTNode;
import org.eclipse.photran.internal.core.vpg.PhotranVPG;
import org.eclipse.photran.internal.ui.editor.FortranEditor;
import org.eclipse.photran.internal.ui.editor_vpg.DefinitionMap;
import org.eclipse.photran.internal.ui.editor_vpg.FortranEditorTasks;
import org.eclipse.photran.internal.ui.editor_vpg.IFortranEditorASTTask;
import org.eclipse.photran.internal.ui.editor_vpg.IFortranEditorVPGTask;

/**
 * 
 * @author seanhurley
 */
public class ASTTest implements IFortranEditorASTTask, IFortranEditorVPGTask
{
    private FortranEditor editor;

    public void setup(FortranEditor editor)
    {
        this.editor = editor;
        FortranEditorTasks.instance(editor).addASTTask(this);
        FortranEditorTasks.instance(editor).addVPGTask(this);
    }

    @Override
    public boolean handle(ASTExecutableProgramNode ast, TokenList tokenList,
        DefinitionMap<Definition> defMap)
    {
        return false;
    }

    @Override
    public void handle(IFile file, IFortranAST ast, DefinitionMap<Definition> defMap)
    {
        System.out.println("---New Scan---");
        ArrayList<ASTCallStmtNode> unsafeCalls = new ArrayList<ASTCallStmtNode>();
        Iterable<? extends IASTNode> rootNodes = ast.getRoot().getChildren();
        for(IASTNode rootNode : rootNodes) {
            ASTListNode<IASTNode> nodeList = (ASTListNode<IASTNode>)rootNode;
            for(IASTNode node : nodeList) {
                unsafeCalls.addAll(TypesafeVisitor.getUnsafeCalls(node));
            }
        }
    }
}
