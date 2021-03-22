# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2021, Science and Technology Facilities Council.
# All rights reserved.
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
# Author J. Henrichs, Bureau of Meteorology
# Modified by A. R. Porter, S. Siso and R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module provides support for adding profiling to code
    generated by PSyclone. '''

from __future__ import absolute_import, print_function
from fparser.common.readfortran import FortranStringReader
from fparser.common.sourceinfo import FortranFormat
from fparser.two import Fortran2003
from psyclone.psyir.nodes.codeblock import CodeBlock
from psyclone.psyir.nodes.psy_data_node import PSyDataNode
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.symbols import (ContainerSymbol, DataSymbol, TypeSymbol,
                                    GlobalInterface, DeferredType,
                                    INTEGER_TYPE)


class ProfileNode(PSyDataNode):
    '''This class can be inserted into a schedule to create profiling code.

    :param ast: reference into the fparser2 parse tree corresponding to \
        this node.
    :type ast: sub-class of :py:class:`fparser.two.Fortran2003.Base`
    :param children: a list of child nodes for this node. These will be made \
        children of the child Schedule of this Profile Node.
    :type children: list of :py::class::`psyclone.psyir.nodes.Node` \
        or derived classes
    :param parent: the parent of this node in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param options: a dictionary with options for transformations.
    :type options: dictionary of string:values or None
    :param str options["prefix"]: The PSyData prefix to use. This string \
        is a prefix attached to all PSyData-related symbols. Defaults \
        to "profile".
    :param (str,str) options["region_name"]: an optional name for this \
        profile region provided as a 2-tuple containing a module name \
        followed by a local name. The pair of strings should uniquely \
        identify a region unless aggregate information is required.

    '''
    _text_name = "Profile"
    _colour = "green"

    def __init__(self, ast=None, children=None, parent=None, options=None):
        if options:
            my_options = options.copy()
        else:
            my_options = {}
        # If there is no value specified in the constructor, default
        # to the "profile" prefix.
        my_options["prefix"] = my_options.get("prefix", "profile")

        super(ProfileNode, self).__init__(ast=ast, children=children,
                                          parent=parent, options=my_options)

    # -------------------------------------------------------------------------
    def __str__(self):
        ''' Returns a string representation of the subtree starting at
        this node. '''
        result = "ProfileStart[var={0}]\n".format(self._var_name)
        for child in self.profile_body.children:
            result += str(child)+"\n"
        return result+"ProfileEnd"

    # -------------------------------------------------------------------------
    @property
    def profile_body(self):
        '''
        :returns: the Schedule associated with this Profiling region.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`

        :raises InternalError: if this Profile node does not have a Schedule \
                               as its one and only child.
        '''
        from psyclone.psyir.nodes import Schedule
        from psyclone.errors import InternalError
        if len(self.children) != 1 or not \
           isinstance(self.children[0], Schedule):
            raise InternalError(
                "ProfileNode malformed or incomplete. It should have a single "
                "Schedule as a child but found: {0}".format(
                    [type(child).__name__ for child in self.children]))
        return super(ProfileNode, self).psy_data_body

    # -------------------------------------------------------------------------
    def gen_code(self, parent):
        # pylint: disable=arguments-differ
        '''Creates the profile start and end calls, surrounding the children
        of this node.

        :param parent: the parent of this node.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        '''
        options = {'pre_var_list': [],
                   'post_var_list': []}

        super(ProfileNode, self).gen_code(parent, options)

    # -------------------------------------------------------------------------
    def gen_c_code(self, indent=0):
        '''
        Generates a string representation of this Node using C language
        (currently not supported).

        :param int indent: Depth of indent for the output string.
        :raises NotImplementedError: Not yet supported for profiling.
        '''
        raise NotImplementedError("Generation of C code is not supported "
                                  "for profiling")

    # -------------------------------------------------------------------------
    def lower_to_language_level(self):
        '''
        Lowers this node (and all children) to language-level PSyIR. The
        PSyIR tree is modified in-place.

        '''
        for child in self.children:
            child.lower_to_language_level()

        if self._module_name:
            name = self._module_name
        else:
            name = self.ancestor(Routine).name
        routine_name = name # Literal(name, CHARACTER_TYPE)

        symbol_table = self.scope.symbol_table
        # Ensure that we have a container symbol for the API access
        fortran_module = self.add_psydata_class_prefix(self.fortran_module)
        try:
            csym = symbol_table.lookup(fortran_module)
        except KeyError:
            csym = ContainerSymbol(fortran_module)
            symbol_table.add(csym)

        for symbol in self.symbols:
            try:
                sym = symbol_table.lookup(symbol)
                # assert sym.interface = 
            except KeyError:
                sym = DataSymbol(symbol, INTEGER_TYPE,
                                 interface=GlobalInterface(csym))
                symbol_table.add(sym)

        # The type symbol for PSyData variables
        var_name = self.add_psydata_class_prefix("PSyDataType")
        try:
            type_sym = symbol_table.lookup(var_name)
        except KeyError:
            type_sym = TypeSymbol(var_name, DeferredType(),
                                  interface=GlobalInterface(csym))

        # Create a name for this region
        if self._region_name:
            name = self._region_name
        else:
            name = "rTODO"
        region_name = name #Literal(name, CHARACTER_TYPE)

        var_name = self.add_psydata_class_prefix("psy_data")

        # Create a symbol for this PSyData region
        # TODO This symbol needs the 'save' and 'target' attributes in Fortran
        dsym = symbol_table.new_symbol(var_name, symbol_type=DataSymbol,
                                       datatype=type_sym)

        # PSyData end call
        reader = FortranStringReader(
            "CALL {0}%PostEnd".format(var_name))
        # Tell the reader that the source is free format
        reader.set_format(FortranFormat(True, False))
        pecall = Fortran2003.Call_Stmt(reader)
        end_call = CodeBlock([pecall], CodeBlock.Structure.STATEMENT)
        self.parent.children.insert(self.position+1, end_call)
        end_call.parent = self.parent

        # PSyData start call (replaces existing PSyDataNode)
        reader = FortranStringReader(
            "CALL {2}%PreStart('{0}', '{1}', 0, 0)".format(
                routine_name, region_name, var_name))
        reader.set_format(FortranFormat(True, False))
        pscall = Fortran2003.Call_Stmt(reader)
        start_call = CodeBlock([pscall], CodeBlock.Structure.STATEMENT)

        for child in self.children[0].pop_all_children():
            self.parent.children.insert(self.position+1, child)
            child.parent = self.parent
        self.replace_with(start_call)
