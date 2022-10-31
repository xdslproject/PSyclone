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
    scalar_type = ScalarType(ScalarType.Intrinsic.REAL, 8)

    array_type=ArrayType(scalar_type, [256, 256])
    array_symbol = symbol_table.new_symbol(root_name="data", symbol_type=DataSymbol, datatype=array_type, interface=ArgumentInterface(ArgumentInterface.Access.READWRITE))

    symbol_table.specify_argument_list([array_symbol])

    index_i_symbol = symbol_table.new_symbol(root_name="i",
                                           symbol_type=DataSymbol,
                                           datatype=INTEGER4_TYPE)

    index_j_symbol = symbol_table.new_symbol(root_name="j",
                                           symbol_type=DataSymbol,
                                           datatype=INTEGER4_TYPE)

    index_k_symbol = symbol_table.new_symbol(root_name="k",
                                           symbol_type=DataSymbol,
                                           datatype=INTEGER4_TYPE)

    # Make generators for nodes which do not have other Nodes as children,
    # with some predefined scalar datatypes

    def zero():
        return Literal("0.0", REAL_TYPE)

    def one():
        return Literal("1", INTEGER_SINGLE_TYPE)

    def float_ten():
        return Literal("10", REAL_DOUBLE_TYPE)

    def loop_max():
        return Literal("255", INTEGER_SINGLE_TYPE)

    def loop_top():
        return Literal("256", INTEGER_SINGLE_TYPE)

    def onert():
        return Literal("1.0", REAL_DOUBLE_TYPE)

    def zerotwofive():
        return Literal("0.25", REAL_DOUBLE_TYPE)

    def int_zero():
        return Literal("0", INTEGER_SINGLE_TYPE)

    def float_one():
        return Literal("1.0", REAL_DOUBLE_TYPE)

    def two():
        return Literal("2", INTEGER_SINGLE_TYPE)

    def num_its():
        return Literal("1000", INTEGER_SINGLE_TYPE)

    # Binary Operation
    im1_j=BinaryOperation.create(BinaryOperation.Operator.SUB, Reference(index_i_symbol), one())
    ip1_j=BinaryOperation.create(BinaryOperation.Operator.ADD, Reference(index_i_symbol), one())
    i_jm1=BinaryOperation.create(BinaryOperation.Operator.SUB, Reference(index_j_symbol), one())
    i_jp1=BinaryOperation.create(BinaryOperation.Operator.ADD, Reference(index_j_symbol), one())

    bop_3=BinaryOperation.create(BinaryOperation.Operator.ADD, ArrayReference.create(array_symbol, [Reference(index_j_symbol), im1_j]), ArrayReference.create(array_symbol, [Reference(index_j_symbol), ip1_j]))
    bop_2=BinaryOperation.create(BinaryOperation.Operator.ADD, bop_3, ArrayReference.create(array_symbol, [i_jm1, Reference(index_i_symbol)]))
    bop_1=BinaryOperation.create(BinaryOperation.Operator.ADD, bop_2, ArrayReference.create(array_symbol, [i_jp1, Reference(index_i_symbol)]))
    binaryoperation = BinaryOperation.create(BinaryOperation.Operator.MUL, bop_1, zerotwofive())

    # Unary Operation
    uoper = UnaryOperation.Operator.SQRT
    unaryoperation=UnaryOperation.create(uoper, onert())

    # Assignments
    #assign1 = Assignment.create(Reference(scalar_symbol), binaryoperation)
    assign2 = Assignment.create(ArrayReference.create(array_symbol, [Reference(index_j_symbol), Reference(index_i_symbol)]), binaryoperation)
    #assign3 = Assignment.create(Reference(scalar_symbol), two())

    loop_inner = Loop.create(index_j_symbol, two(), loop_max(), one(),
                       [assign2])

    # Loop
    loop_outer = Loop.create(index_i_symbol, two(), loop_max(), one(),
                       [loop_inner])

    loop_its = Loop.create(index_k_symbol, one(), num_its(), one(),
                       [loop_outer])

    # Routine
    routine = Routine.create("gauss_seidel", symbol_table, [loop_its], False)

    # Container
    container_symbol_table = SymbolTable()
    container = Container.create("CONTAINER", container_symbol_table, [routine])

    # Routine (specified as being a program)
    program_symbol_table = SymbolTable()
    work_symbol = RoutineSymbol("gauss_seidel", REAL_TYPE)
    container_symbol = ContainerSymbol("CONTAINER")
    work_symbol.interface = ImportInterface(container_symbol)

    program_symbol_table.add(container_symbol)
    program_symbol_table.add(work_symbol)
    data_symbol = program_symbol_table.new_symbol(root_name="data", symbol_type=DataSymbol, datatype=array_type)

    declaration_index_i_symbol = program_symbol_table.new_symbol(root_name="i",
                                           symbol_type=DataSymbol,
                                           datatype=INTEGER4_TYPE)

    declaration_index_j_symbol = program_symbol_table.new_symbol(root_name="j",
                                           symbol_type=DataSymbol,
                                           datatype=INTEGER4_TYPE)

    assign = Assignment.create(ArrayReference.create(data_symbol, [Reference(declaration_index_j_symbol), Reference(declaration_index_i_symbol)]), zero())

    declaration_loop_inner = Loop.create(declaration_index_j_symbol, two(), loop_max(), one(),
                       [assign])

    # Loop
    declaration_loop_outer = Loop.create(declaration_index_i_symbol, two(), loop_max(), one(),
                       [declaration_loop_inner])

    assign_low_bc = Assignment.create(ArrayReference.create(data_symbol, [Reference(declaration_index_j_symbol), one()]), float_one())
    low_bc_loop=Loop.create(declaration_index_j_symbol, two(), loop_max(), one(),
                       [assign_low_bc])

    assign_high_bc = Assignment.create(ArrayReference.create(data_symbol, [Reference(declaration_index_j_symbol), loop_top()]), float_ten())
    assign_high_bc=Loop.create(declaration_index_j_symbol, two(), loop_max(), one(),
                       [assign_high_bc])

    call = Call.create(work_symbol, [Reference(data_symbol)])
    #assign = Assignment.create(Reference(assgn_symbol), call)
    program = Routine.create("some_program", program_symbol_table, [declaration_loop_outer, low_bc_loop, assign_high_bc, call], is_program=True)

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

