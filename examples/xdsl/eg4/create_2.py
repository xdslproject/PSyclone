# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, xDSL project
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------

'''An example which builds a subroutine as a function, a loop and 
conditional and then generates the xDSL IR along with Fortran code

Once you have psyclone installed, this script may be run by doing:

>>> python create.py

This should output the xDSL IR representation and Fortran code

'''
from __future__ import print_function
from psyclone.psyir.nodes import Reference, Literal, UnaryOperation, \
    BinaryOperation, NaryOperation, Assignment, IfBlock, Loop, Return, \
    Container, Range, ArrayReference, Call, Routine, FileContainer
from psyclone.psyir.symbols import DataSymbol, RoutineSymbol, SymbolTable, \
    ContainerSymbol, ArgumentInterface, ScalarType, ArrayType, \
    ImportInterface, REAL_TYPE, REAL4_TYPE, REAL_DOUBLE_TYPE, INTEGER_TYPE, \
    INTEGER_SINGLE_TYPE, INTEGER4_TYPE, INTEGER8_TYPE
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.backend.c import CWriter
from psyclone.psyir.backend.xdsl import xDSLWriter
from xdsl.printer import Printer
import sys

def create_psyir_tree():
    ''' Create an example PSyIR Tree.

    '''
    # Symbol table, symbols and scalar datatypes
    symbol_table = SymbolTable()

    # Array using precision defined by another symbol
    scalar_type = ScalarType(ScalarType.Intrinsic.INTEGER, 4)
    scalar_symbol = symbol_table.new_symbol(root_name="a", symbol_type=DataSymbol, datatype=scalar_type)

    tmp_symbol = symbol_table.new_symbol(root_name="tmp_data", symbol_type=DataSymbol,
                                         datatype=REAL_TYPE)

    index_symbol = symbol_table.new_symbol(root_name="i",
                                           symbol_type=DataSymbol,
                                           datatype=INTEGER4_TYPE)

    rq_symbol = symbol_table.new_symbol(symbol_type=DataSymbol,
                                           datatype=INTEGER4_TYPE,
                                           interface=ArgumentInterface(ArgumentInterface.Access.READ))
    symbol_table.specify_argument_list([rq_symbol])

    # Make generators for nodes which do not have other Nodes as children,
    # with some predefined scalar datatypes

    def zero():
        return Literal("0.0", REAL_TYPE)

    def one():
        return Literal("1", INTEGER_SINGLE_TYPE)

    def onert():
        return Literal("1.0", REAL_DOUBLE_TYPE)

    def int_zero():
        return Literal("0", INTEGER_SINGLE_TYPE)

    def int_one():
        return Literal("1", INTEGER8_TYPE)

    def two():
        return Literal("2", INTEGER_SINGLE_TYPE)

    # Binary Operation
    oper = BinaryOperation.Operator.ADD
    binaryoperation = BinaryOperation.create(oper, one(), two())
    
    # Unary Operation
    uoper = UnaryOperation.Operator.SQRT
    unaryoperation=UnaryOperation.create(uoper, onert())

    # Assignments
    assign1 = Assignment.create(Reference(scalar_symbol), binaryoperation)
    assign2 = Assignment.create(Reference(tmp_symbol), onert())
    assign3 = Assignment.create(Reference(scalar_symbol), two())

    # Loop
    loop = Loop.create(index_symbol, int_zero(), int_one(), int_one(),
                       [assign2])

    if_condition = BinaryOperation.create(BinaryOperation.Operator.EQ,
                                          Reference(scalar_symbol), int_one())

    not_condition = UnaryOperation.create(UnaryOperation.Operator.NOT, if_condition)
                                          
    ifblock = IfBlock.create(not_condition, [loop]) 
    
    retrn = Return()

    # Routine    
    routine = Routine.create("work", symbol_table, [assign1, assign3, ifblock, retrn], False, "tmp_data")

    # Container
    container_symbol_table = SymbolTable()
    container = Container.create("CONTAINER", container_symbol_table, [routine])

    # Routine (specified as being a program)
    program_symbol_table = SymbolTable()
    work_symbol = RoutineSymbol("work", REAL_TYPE)
    container_symbol = ContainerSymbol("CONTAINER")
    work_symbol.interface = ImportInterface(container_symbol)

    program_symbol_table.add(container_symbol)
    program_symbol_table.add(work_symbol)
    assgn_symbol = program_symbol_table.new_symbol(root_name="a", symbol_type=DataSymbol, datatype=scalar_type)
    call = Call.create(work_symbol, [two()])
    assign = Assignment.create(Reference(assgn_symbol), call)
    
    program = Routine.create("some_program", program_symbol_table, [assign], is_program=True)

    # File container
    file_container = FileContainer.create("dummy", SymbolTable(), [container, program])

    return file_container


if __name__ == "__main__":
    psyir_tree = create_psyir_tree()

    writer = xDSLWriter()
    result = writer(psyir_tree)

    printer = Printer(stream=sys.stdout)
    printer.print_op(result)

    # Write out the code as Fortran
    writer = FortranWriter()
    result = writer(psyir_tree)
    print(result)
  