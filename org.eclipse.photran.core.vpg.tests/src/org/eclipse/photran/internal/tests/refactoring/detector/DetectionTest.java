/*******************************************************************************
 * Copyright (c) 2012 TODO COMPANY NAME and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    DFL (TODO COMPANY NAME) - Initial API and implementation
 *******************************************************************************/
package org.eclipse.photran.internal.tests.refactoring.detector;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import org.eclipse.photran.internal.core.SyntaxException;
import org.eclipse.photran.internal.core.lexer.ASTLexerFactory;
import org.eclipse.photran.internal.core.lexer.IAccumulatingLexer;
import org.eclipse.photran.internal.core.lexer.LexerException;
import org.eclipse.photran.internal.core.lexer.Token;
import org.eclipse.photran.internal.core.parser.ASTExecutableProgramNode;
import org.eclipse.photran.internal.core.parser.IASTListNode;
import org.eclipse.photran.internal.core.parser.IProgramUnit;
import org.eclipse.photran.internal.core.parser.Parser;
import org.eclipse.photran.internal.ui.editor_vpg.lint.TypesafeCallVisitor;

import junit.framework.TestCase;

/**
 * 
 * @author seanhurley
 */
public class DetectionTest extends TestCase
{
    String DIR = "refactoring-test-code/create-interface"; 
    String[] files = {"/basic/test-basic.f90"};  
    String[] filenames; 
    int SIZE = 1;
    
    @Override
    public void setUp()
    {
        filenames = new String[SIZE]; 
        for(int i = 0; i < SIZE; i++)
            filenames[i] = DIR+files[i]; 
    }
    
    
    public void testBasic() throws IOException, LexerException, SyntaxException 
    { 
        //get the IASTNode
        File file = new File(filenames[0]); 
        ASTLexerFactory alf = new ASTLexerFactory(); 
        IAccumulatingLexer ial = alf.createLexer(file); 
        Parser p = new Parser(); 
        ASTExecutableProgramNode aepn = p.parse(ial); 
        IASTListNode<IProgramUnit> nodes = aepn.getProgramUnitList(); 
        
        //Apply the Typesafe detection
        ArrayList<Token> detected = TypesafeCallVisitor.getUnsafeCalls(nodes.get(0)); 
        
        //check that it is as expected
        int size = detected.size(); 
        int[] lineNums = new int[size]; 
        
        for(int i = 0; i < detected.size(); i++) 
        {
            Token token = detected.get(i); 
            lineNums[i] = token.getLine();            
        }
        
        assertTrue(size == 1); 
        assertTrue(lineNums[0] == 10);    
    }
}
