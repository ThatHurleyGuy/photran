/*******************************************************************************
 * Copyright (c) 2012 TODO COMPANY NAME and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *    Dennis Lin - Initial API and implementation
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
 * Test suite for testing the detection of non typesafe calls
 * @author Dennis Lin, seanhurley
 */
public class DetectionTest extends TestCase
{ 
    //test files
    String[] filenames = {"refactoring-test-code/introduce-interface/basic/test-basic.f90", 
                          "refactoring-test-code/typesafe-detection/intrinsics/test-intrinsic.f90",
                          "refactoring-test-code/typesafe-detection/multiple/test-multiple.f90"};      
    
    //tests a basic case
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
    
    //makes sure it doesn't flag intrinsics
    public void testIntrinsics() throws IOException, LexerException, SyntaxException
    {
        //get the IASTNode
        File file = new File(filenames[1]); 
        ASTLexerFactory alf = new ASTLexerFactory(); 
        IAccumulatingLexer ial = alf.createLexer(file); 
        Parser p = new Parser(); 
        ASTExecutableProgramNode aepn = p.parse(ial); 
        IASTListNode<IProgramUnit> nodes = aepn.getProgramUnitList(); 
        
        //Apply the Typesafe detection
        ArrayList<Token> detected = TypesafeCallVisitor.getUnsafeCalls(nodes.get(0)); 
        
        //check that it is as expected
        assertTrue(detected.isEmpty());
    }

    //makes sure it can flag multiple
    public void testMultiple() throws IOException, LexerException, SyntaxException
    {
        //get the IASTNode
        File file = new File(filenames[2]); 
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
        
        assertTrue(size == 2); 
        
        //not always in the same order
        if (lineNums[0] == 6) 
            assertTrue(lineNums[1] == 8); 
        else if (lineNums[0] == 8)
            assertTrue(lineNums[1] == 6); 
        else 
            assertTrue(false); 
    }
}

