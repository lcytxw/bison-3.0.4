/* Print information on generated parser, for bison,

   Copyright (C) 1984, 1986, 1989, 2000-2005, 2007, 2009-2015 Free
   Software Foundation, Inc.

   This file is part of Bison, the GNU Compiler Compiler.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>
#include "system.h"

#include <bitset.h>

#include "LR0.h"
#include "closure.h"
#include "conflicts.h"
#include "files.h"
#include "getargs.h"
#include "gram.h"
#include "lalr.h"
#include "muscle-tab.h"
#include "print.h"
#include "reader.h"
#include "reduce.h"
#include "state.h"
#include "symtab.h"
#include "tables.h"

static bitset no_reduce_set;

#include <bitset_stats.h>
#include <configmake.h>
#include <progname.h>
#include <quotearg.h>
#include <timevar.h>

#include "LR0.h"
#include "closeout.h"
#include "complain.h"
#include "derives.h"
#include "ielr.h"
#include "nullable.h"
#include "output.h"
#include "print_graph.h"
#include "print-xml.h"
#include <quote.h>
#include "scan-code.h"
#include "scan-gram.h"
#include "scan-skel.h"
#include "tables.h"
#include "uniqstr.h"

int
fuzz_main (int argc, char *argv[])
{
  set_program_name (argv[0]);
  setlocale (LC_ALL, "");
  (void) bindtextdomain (PACKAGE, LOCALEDIR);
  (void) bindtextdomain ("bison-runtime", LOCALEDIR);
  (void) textdomain (PACKAGE);

  {
    char const *cp = getenv ("LC_CTYPE");
    if (cp && STREQ (cp, "C"))
      set_custom_quoting (&quote_quoting_options, "'", "'");
    else
      set_quoting_style (&quote_quoting_options, locale_quoting_style);
  }

  atexit (close_stdout);

  uniqstrs_new ();
  muscle_init ();
  complain_init ();

  getargs (argc, argv);

  timevar_report = trace_flag & trace_time;
  init_timevar ();
  timevar_start (TV_TOTAL);

  if (trace_flag & trace_bitsets)
    bitset_stats_enable ();

  /* Read the input.  Copy some parts of it to FGUARD, FACTION, FTABLE
     and FATTRS.  In file reader.c.  The other parts are recorded in
     the grammar; see gram.h.  */

  timevar_push (TV_READER);
  reader ();
  timevar_pop (TV_READER);

  if (complaint_status == status_complaint)
    goto finish;

  /* Find useless nonterminals and productions and reduce the grammar. */
  timevar_push (TV_REDUCE);
  reduce_grammar ();
  timevar_pop (TV_REDUCE);

  /* Record other info about the grammar.  In files derives and
     nullable.  */
  timevar_push (TV_SETS);
  derives_compute ();
  nullable_compute ();
  timevar_pop (TV_SETS);

  /* Compute LR(0) parser states.  See state.h for more info.  */
  timevar_push (TV_LR0);
  generate_states ();
  timevar_pop (TV_LR0);

  /* Add lookahead sets to parser states.  Except when LALR(1) is
     requested, split states to eliminate LR(1)-relative
     inadequacies.  */
  ielr ();

  /* Find and record any conflicts: places where one token of
     lookahead is not enough to disambiguate the parsing.  In file
     conflicts.  Also resolve s/r conflicts based on precedence
     declarations.  */
  timevar_push (TV_CONFLICTS);
  conflicts_solve ();
  if (!muscle_percent_define_flag_if ("lr.keep-unreachable-state"))
    {
      state_number *old_to_new = xnmalloc (nstates, sizeof *old_to_new);
      state_number nstates_old = nstates;
      state_remove_unreachable_states (old_to_new);
      lalr_update_state_numbers (old_to_new, nstates_old);
      conflicts_update_state_numbers (old_to_new, nstates_old);
      free (old_to_new);
    }
  conflicts_print ();
  timevar_pop (TV_CONFLICTS);

  /* Compute the parser tables.  */
  timevar_push (TV_ACTIONS);
  tables_generate ();
  timevar_pop (TV_ACTIONS);

  grammar_rules_useless_report (_("rule useless in parser due to conflicts"));

  print_precedence_warnings ();

  /* Output file names. */
  compute_output_file_names ();

  /* Output the detailed report on the grammar.  */
  if (report_flag)
    {
      timevar_push (TV_REPORT);
      print_results ();
      timevar_pop (TV_REPORT);
    }

  /* Output the graph.  */
  if (graph_flag)
    {
      timevar_push (TV_GRAPH);
      print_graph ();
      timevar_pop (TV_GRAPH);
    }

  /* Output xml.  */
  if (xml_flag)
    {
      timevar_push (TV_XML);
      print_xml ();
      timevar_pop (TV_XML);
    }

  /* Stop if there were errors, to avoid trashing previous output
     files.  */
  if (complaint_status == status_complaint)
    goto finish;

  /* Lookahead tokens are no longer needed. */
  timevar_push (TV_FREE);
  lalr_free ();
  timevar_pop (TV_FREE);

  /* Output the tables and the parser to ftable.  In file output.  */
  timevar_push (TV_PARSER);
  output ();
  timevar_pop (TV_PARSER);

  timevar_push (TV_FREE);
  nullable_free ();
  derives_free ();
  tables_free ();
  states_free ();
  reduce_free ();
  conflicts_free ();
  grammar_free ();
  output_file_names_free ();

  /* The scanner memory cannot be released right after parsing, as it
     contains things such as user actions, prologue, epilogue etc.  */
  gram_scanner_free ();
  muscle_free ();
  uniqstrs_free ();
  code_scanner_free ();
  skel_scanner_free ();
  quotearg_free ();
  timevar_pop (TV_FREE);

  if (trace_flag & trace_bitsets)
    bitset_stats_dump (stderr);

 finish:

  /* Stop timing and print the times.  */
  timevar_stop (TV_TOTAL);
  timevar_print (stderr);

  cleanup_caret ();

  return complaint_status ? EXIT_FAILURE : EXIT_SUCCESS;
}

#if 0
static void
print_token (int extnum, int token)
{
  fprintf (out, _(" type %d is %s\n"), extnum, tags[token]);
}
#endif



/*---------------------------------------.
| *WIDTH := max (*WIDTH, strlen (STR)).  |
`---------------------------------------*/

static void
max_length (size_t *width, const char *str)
{
  size_t len = strlen (str);
  if (len > *width)
    *width = len;
}

/*--------------------------------.
| Report information on a state.  |
`--------------------------------*/

static void
print_core (FILE *out, state *s)
{
  size_t i;
  item_number *sitems = s->items;
  size_t snritems = s->nitems;
  symbol *previous_lhs = NULL;

  /* Output all the items of a state, not only its kernel.  */
  if (report_flag & report_itemsets)
    {
      closure (sitems, snritems);
      sitems = itemset;
      snritems = nitemset;
    }

  if (!snritems)
    return;

  fputc ('\n', out);

  for (i = 0; i < snritems; i++)
    {
      item_number *sp;
      item_number *sp1;
      rule_number r;

      sp1 = sp = ritem + sitems[i];

      while (*sp >= 0)
        sp++;

      r = item_number_as_rule_number (*sp);

      rule_lhs_print (&rules[r], previous_lhs, out);
      previous_lhs = rules[r].lhs;

      for (sp = rules[r].rhs; sp < sp1; sp++)
        fprintf (out, " %s", symbols[*sp]->tag);
      fputs (" .", out);
      if (0 <= *rules[r].rhs)
        for (/* Nothing */; 0 <= *sp; ++sp)
          fprintf (out, " %s", symbols[*sp]->tag);
      else
        fprintf (out, " %%empty");

      /* Display the lookahead tokens?  */
      if (report_flag & report_lookahead_tokens
          && item_number_is_rule_number (*sp1))
        state_rule_lookahead_tokens_print (s, &rules[r], out);

      fputc ('\n', out);
    }
}


/*------------------------------------------------------------.
| Report the shifts iff DISPLAY_SHIFTS_P or the gotos of S on |
| OUT.                                                        |
`------------------------------------------------------------*/

static void
print_transitions (state *s, FILE *out, bool display_transitions_p)
{
  transitions *trans = s->transitions;
  size_t width = 0;
  int i;

  /* Compute the width of the lookahead token column.  */
  for (i = 0; i < trans->num; i++)
    if (!TRANSITION_IS_DISABLED (trans, i)
        && TRANSITION_IS_SHIFT (trans, i) == display_transitions_p)
      {
        symbol *sym = symbols[TRANSITION_SYMBOL (trans, i)];
        max_length (&width, sym->tag);
      }

  /* Nothing to report. */
  if (!width)
    return;

  fputc ('\n', out);
  width += 2;

  /* Report lookahead tokens and shifts.  */
  for (i = 0; i < trans->num; i++)
    if (!TRANSITION_IS_DISABLED (trans, i)
        && TRANSITION_IS_SHIFT (trans, i) == display_transitions_p)
      {
        symbol *sym = symbols[TRANSITION_SYMBOL (trans, i)];
        const char *tag = sym->tag;
        state *s1 = trans->states[i];
        int j;

        fprintf (out, "    %s", tag);
        for (j = width - strlen (tag); j > 0; --j)
          fputc (' ', out);
        if (display_transitions_p)
          fprintf (out, _("shift, and go to state %d\n"), s1->number);
        else
          fprintf (out, _("go to state %d\n"), s1->number);
      }
}


/*--------------------------------------------------------.
| Report the explicit errors of S raised from %nonassoc.  |
`--------------------------------------------------------*/

static void
print_errs (FILE *out, state *s)
{
  errs *errp = s->errs;
  size_t width = 0;
  int i;

  /* Compute the width of the lookahead token column.  */
  for (i = 0; i < errp->num; ++i)
    if (errp->symbols[i])
      max_length (&width, errp->symbols[i]->tag);

  /* Nothing to report. */
  if (!width)
    return;

  fputc ('\n', out);
  width += 2;

  /* Report lookahead tokens and errors.  */
  for (i = 0; i < errp->num; ++i)
    if (errp->symbols[i])
      {
        const char *tag = errp->symbols[i]->tag;
        int j;
        fprintf (out, "    %s", tag);
        for (j = width - strlen (tag); j > 0; --j)
          fputc (' ', out);
        fputs (_("error (nonassociative)\n"), out);
      }
}


/*-------------------------------------------------------------------------.
| Report a reduction of RULE on LOOKAHEAD_TOKEN (which can be 'default').  |
| If not ENABLED, the rule is masked by a shift or a reduce (S/R and       |
| R/R conflicts).                                                          |
`-------------------------------------------------------------------------*/

static void
print_reduction (FILE *out, size_t width,
                 const char *lookahead_token,
                 rule *r, bool enabled)
{
  int j;
  fprintf (out, "    %s", lookahead_token);
  for (j = width - strlen (lookahead_token); j > 0; --j)
    fputc (' ', out);
  if (!enabled)
    fputc ('[', out);
  if (r->number)
    fprintf (out, _("reduce using rule %d (%s)"), r->number, r->lhs->tag);
  else
    fprintf (out, _("accept"));
  if (!enabled)
    fputc (']', out);
  fputc ('\n', out);
}


/*-------------------------------------------.
| Report on OUT the reduction actions of S.  |
`-------------------------------------------*/

static void
print_reductions (FILE *out, state *s)
{
  transitions *trans = s->transitions;
  reductions *reds = s->reductions;
  rule *default_reduction = NULL;
  size_t width = 0;
  int i, j;
  bool default_reduction_only = true;

  if (reds->num == 0)
    return;

  if (yydefact[s->number] != 0)
    default_reduction = &rules[yydefact[s->number] - 1];

  bitset_zero (no_reduce_set);
  FOR_EACH_SHIFT (trans, i)
    bitset_set (no_reduce_set, TRANSITION_SYMBOL (trans, i));
  for (i = 0; i < s->errs->num; ++i)
    if (s->errs->symbols[i])
      bitset_set (no_reduce_set, s->errs->symbols[i]->number);

  /* Compute the width of the lookahead token column.  */
  if (default_reduction)
    width = strlen (_("$default"));

  if (reds->lookahead_tokens)
    for (i = 0; i < ntokens; i++)
      {
        bool count = bitset_test (no_reduce_set, i);

        for (j = 0; j < reds->num; ++j)
          if (bitset_test (reds->lookahead_tokens[j], i))
            {
              if (! count)
                {
                  if (reds->rules[j] != default_reduction)
                    max_length (&width, symbols[i]->tag);
                  count = true;
                }
              else
                {
                  max_length (&width, symbols[i]->tag);
                }
            }
      }

  /* Nothing to report. */
  if (!width)
    return;

  fputc ('\n', out);
  width += 2;

  /* Report lookahead tokens (or $default) and reductions.  */
  if (reds->lookahead_tokens)
    for (i = 0; i < ntokens; i++)
      {
        bool defaulted = false;
        bool count = bitset_test (no_reduce_set, i);
        if (count)
          default_reduction_only = false;

        for (j = 0; j < reds->num; ++j)
          if (bitset_test (reds->lookahead_tokens[j], i))
            {
              if (! count)
                {
                  if (reds->rules[j] != default_reduction)
                    {
                      default_reduction_only = false;
                      print_reduction (out, width,
                                       symbols[i]->tag,
                                       reds->rules[j], true);
                    }
                  else
                    defaulted = true;
                  count = true;
                }
              else
                {
                  default_reduction_only = false;
                  if (defaulted)
                    print_reduction (out, width,
                                     symbols[i]->tag,
                                     default_reduction, true);
                  defaulted = false;
                  print_reduction (out, width,
                                   symbols[i]->tag,
                                   reds->rules[j], false);
                }
            }
      }

  if (default_reduction)
    {
      char *default_reductions =
        muscle_percent_define_get ("lr.default-reduction");
      print_reduction (out, width, _("$default"), default_reduction, true);
      aver (STREQ (default_reductions, "most")
            || (STREQ (default_reductions, "consistent")
                && default_reduction_only)
            || (reds->num == 1 && reds->rules[0]->number == 0));
      (void) default_reduction_only;
      free (default_reductions);
    }
}


/*--------------------------------------------------------------.
| Report on OUT all the actions (shifts, gotos, reductions, and |
| explicit erros from %nonassoc) of S.                          |
`--------------------------------------------------------------*/

static void
print_actions (FILE *out, state *s)
{
  /* Print shifts.  */
  print_transitions (s, out, true);
  print_errs (out, s);
  print_reductions (out, s);
  /* Print gotos.  */
  print_transitions (s, out, false);
}


/*----------------------------------.
| Report all the data on S on OUT.  |
`----------------------------------*/

static void
print_state (FILE *out, state *s)
{
  fputs ("\n\n", out);
  fprintf (out, _("State %d"), s->number);
  fputc ('\n', out);
  print_core (out, s);
  print_actions (out, s);
  if ((report_flag & report_solved_conflicts) && s->solved_conflicts)
    {
      fputc ('\n', out);
      fputs (s->solved_conflicts, out);
    }
}

/*-----------------------------------------.
| Print information on the whole grammar.  |
`-----------------------------------------*/

#define END_TEST(End)                           \
  do {                                          \
    if (column + strlen (buffer) > (End))       \
      {                                         \
        fprintf (out, "%s\n   ", buffer);       \
        column = 3;                             \
        buffer[0] = 0;                          \
      }                                         \
  } while (0)


static void
print_grammar (FILE *out)
{
  symbol_number i;
  char buffer[90];
  int column = 0;

  grammar_rules_print (out);

  /* TERMINAL (type #) : rule #s terminal is on RHS */
  fprintf (out, "%s\n\n", _("Terminals, with rules where they appear"));
  for (i = 0; i < max_user_token_number + 1; i++)
    if (token_translations[i] != undeftoken->number)
      {
        const char *tag = symbols[token_translations[i]]->tag;
        rule_number r;
        item_number *rhsp;

        buffer[0] = 0;
        column = strlen (tag);
        fputs (tag, out);
        END_TEST (65);
        sprintf (buffer, " (%d)", i);

        for (r = 0; r < nrules; r++)
          for (rhsp = rules[r].rhs; *rhsp >= 0; rhsp++)
            if (item_number_as_symbol_number (*rhsp) == token_translations[i])
              {
                END_TEST (65);
                sprintf (buffer + strlen (buffer), " %d", r);
                break;
              }
        fprintf (out, "%s\n", buffer);
      }
  fputs ("\n\n", out);


  fprintf (out, "%s\n\n", _("Nonterminals, with rules where they appear"));
  for (i = ntokens; i < nsyms; i++)
    {
      int left_count = 0, right_count = 0;
      rule_number r;
      const char *tag = symbols[i]->tag;

      for (r = 0; r < nrules; r++)
        {
          item_number *rhsp;
          if (rules[r].lhs->number == i)
            left_count++;
          for (rhsp = rules[r].rhs; *rhsp >= 0; rhsp++)
            if (item_number_as_symbol_number (*rhsp) == i)
              {
                right_count++;
                break;
              }
        }

      buffer[0] = 0;
      fputs (tag, out);
      column = strlen (tag);
      sprintf (buffer, " (%d)", i);
      END_TEST (0);

      if (left_count > 0)
        {
          END_TEST (65);
          sprintf (buffer + strlen (buffer), _(" on left:"));

          for (r = 0; r < nrules; r++)
            {
              if (rules[r].lhs->number == i)
                {
                  END_TEST (65);
                  sprintf (buffer + strlen (buffer), " %d", r);
                }
            }
        }

      if (right_count > 0)
        {
          if (left_count > 0)
            sprintf (buffer + strlen (buffer), ",");
          END_TEST (65);
          sprintf (buffer + strlen (buffer), _(" on right:"));
          for (r = 0; r < nrules; r++)
            {
              item_number *rhsp;
              for (rhsp = rules[r].rhs; *rhsp >= 0; rhsp++)
                if (item_number_as_symbol_number (*rhsp) == i)
                  {
                    END_TEST (65);
                    sprintf (buffer + strlen (buffer), " %d", r);
                    break;
                  }
            }
        }
      fprintf (out, "%s\n", buffer);
    }
}

void
print_results (void)
{
  state_number i;

  /* We used to use just .out if SPEC_NAME_PREFIX (-p) was used, but
     that conflicts with Posix.  */
  FILE *out = xfopen (spec_verbose_file, "w");

  reduce_output (out);
  grammar_rules_partial_print (out,
                               _("Rules useless in parser due to conflicts"),
                                 rule_useless_in_parser_p);
  conflicts_output (out);

  print_grammar (out);

  /* If the whole state item sets, not only the kernels, are wanted,
     'closure' will be run, which needs memory allocation/deallocation.   */
  if (report_flag & report_itemsets)
    new_closure (nritems);
  /* Storage for print_reductions.  */
  no_reduce_set =  bitset_create (ntokens, BITSET_FIXED);
  for (i = 0; i < nstates; i++)
    print_state (out, states[i]);
  bitset_free (no_reduce_set);
  if (report_flag & report_itemsets)
    free_closure ();

  xfclose (out);
}
