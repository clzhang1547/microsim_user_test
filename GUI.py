from tkinter import *
from tkinter import ttk, filedialog, messagebox
import os
import sys
import datetime
import multiprocessing
import ast
import queue
import subprocess
import configparser
import pandas as pd
from abc import ABCMeta, abstractmethod
from _5_simulation import SimulationEngine
from ABF import ABF
from Utils import *
import matplotlib
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from matplotlib.figure import Figure
matplotlib.use("TkAgg")

DARK_COLOR = '#333333'
LIGHT_COLOR = '#f2f2f2'
VERY_LIGHT_COLOR = '#fcfcfc'
THEME_COLOR = '#0074BF'


class MicrosimGUI(Tk):
    def __init__(self, *args, **kwargs):
        """Create main window"""

        super().__init__(*args, **kwargs)
        # Create general parameters object which will be shared across comparison simulations
        self.general_params = GeneralParameters()
        # These are the default parameters object that will be used to fill inputs upon initial load
        self.default_params = OtherParameters()
        self.config_fp = 'config.ini'
        self.config = configparser.ConfigParser()

        self.showing_advanced = False  # Whether or not advanced parameters are being shown

        # When comparing multiple simulations, keep track of each simulation's parameters
        self.all_params = [self.default_params]
        self.comparing = False
        self.current_sim_num = 0

        self.currently_running = False  # Whether or not a simulation is currently running

        self.error_tooltips = []  # Tooltips that are used to tell users when they enter an invalid value

        self.current_tab = 0  # Set the current visible tab to 0, which is the Program tab
        self.variables = self.create_variables()  # Create the variables that will be tied to each input
        self.load_config()  # Load configurations file
        self.set_up_style()  # Set up styles for ttk widgets

        self.title('Paid Leave Micro-Simulator')  # Add title to window
        self.option_add('*Font', '-size 12')  # Set default font
        # self.resizable(False, False)  # Prevent window from being resized
        self.bind("<MouseWheel>", self.scroll)  # Bind mouse wheel action to scroll function

        # Attach icon to application
        self.icon = PhotoImage(file='impaq_logo.gif')
        self.tk.call('wm', 'iconphoto', self._w, self.icon)

        # Need to keep track of the multiple results and progress windows that are created to close them all when
        # exiting program
        self.results_windows = []
        self.progress_windows = []
        self.protocol("WM_DELETE_WINDOW", self.on_close)  # When the user closes the main window, run this function

        # The content frame will hold all widgets
        self.content = Frame(self, padx=15, pady=15, bg=DARK_COLOR)
        # This frame holds general parameters
        self.general_params_frame = GeneralParamsFrame(self.content, bg=DARK_COLOR)
        # This frame holds buttons for comparing program parameters
        self.simulation_comparison = ComparisonFrame(self.content, bg=DARK_COLOR)
        # This notebook will have three tabs for the program, population, and simulation parameters
        self.parameter_notebook = ParameterNotebook(self.content, height=150)
        self.advanced_frame = AdvancedFrame(self.content, self.toggle_advanced_parameters)
        self.run_button = RunButton(self.content, text="Run", height=1, command=self.run_simulation)

        # Add callbacks that will run when certain variables are changed
        self.add_variable_callbacks()

        # ----------------------------------------- Add Widgets to Window --------------------------------------------

        self.content.pack(expand=True, fill=BOTH)
        self.general_params_frame.pack(fill=X)
        self.simulation_comparison.pack(fill=X, pady=(2, 0))
        self.parameter_notebook.pack(expand=True, fill=BOTH, pady=(4, 4))
        self.advanced_frame.pack(anchor=E, pady=(0, 4))
        self.run_button.pack(anchor=E, fill=Y)

        self.update_idletasks()
        self.original_height = self.winfo_height()  # Need to keep track of original height of window when resizing
        self.minsize(self.winfo_width(), 650)  # Set minimum height of window

        # Display the advanced parameters
        try:
            if self.config['PREFERENCES'].getboolean('showing_advanced'):
                self.toggle_advanced_parameters()
        except ValueError:
            self.config['PREFERENCES']['showing_advanced'] = 'False'

        center_window(self)  # Position window in middle of the screen

        self.abf_module = self.sim_engine = self.engine_process = self.current_state = None
        self.check_file_entries()  # Check the file entries on start to disable run button if necessary

    @staticmethod
    def set_up_style():
        """Create the style for ttk widgets. These new styles are given their own names, which will have to be provided
        by the widgets in order to be used."""
        style = ttk.Style()
        style.configure('MSCombobox.TCombobox', relief='flat')
        style.configure('MSCheckbutton.TCheckbutton', background=VERY_LIGHT_COLOR, font='-size 12')
        style.configure('MSCheckbuttonSmall.TCheckbutton', background=VERY_LIGHT_COLOR, font='-size 10 -weight bold')
        style.configure('MSCheckbuttonRed.TCheckbutton', background='red', foreground='white', font='-size 12')
        style.configure('DarkCheckbutton.TCheckbutton', background=DARK_COLOR, foreground=LIGHT_COLOR, font='-size 12')
        style.configure('MSNotebook.TNotebook', background=VERY_LIGHT_COLOR)
        style.configure('MSNotebook.TNotebook.Tab', font='-size 12', padding=(4, 0))
        # style.configure('DarkNotebook.TNotebook', background=DARK_COLOR)
        # style.configure('DarkNotebook.TNotebook.Tab', font='-size 12', padding=(4, 0))
        style.configure('MSLabelframe.TLabelframe', background=VERY_LIGHT_COLOR)
        style.configure('MSLabelframe.TLabelframe.Label', background=VERY_LIGHT_COLOR, foreground=THEME_COLOR,
                        font='-size 12')

    def create_variables(self):
        """Create the variables that the users will update in the interface.
        These variables will be passed to the engine."""
        g = self.general_params
        d = self.default_params
        variables = {
            'fmla_file': StringVar(value=g.fmla_file),
            'fmla_wave': IntVar(value=g.fmla_wave),
            'acs_directory': StringVar(value=g.acs_directory),
            'output_directory': StringVar(value=g.output_directory),
            'state': StringVar(value=g.state),
            'year': IntVar(value=g.year),
            'simulation_method': StringVar(value=g.simulation_method),
            'existing_program': StringVar(),
            'engine_type': StringVar(value=g.engine_type),
            'r_path': StringVar(value=g.r_path),
            'benefit_effect': BooleanVar(value=d.benefit_effect),
            'calibrate': BooleanVar(value=d.calibrate),
            'clone_factor': IntVar(value=d.clone_factor),
            'se_analysis': BooleanVar(value=d.se_analysis),
            'extend': BooleanVar(value=d.extend),
            'fmla_protection_constraint': BooleanVar(value=d.fmla_protection_constraint),
            'replacement_ratio': DoubleVar(value=d.replacement_ratio),
            'own_health': BooleanVar(value=d.own_health),
            'maternity': BooleanVar(value=d.maternity),
            'new_child': BooleanVar(value=d.new_child),
            'ill_child': BooleanVar(value=d.ill_child),
            'ill_spouse': BooleanVar(value=d.ill_spouse),
            'ill_parent': BooleanVar(value=d.ill_parent),
            'private': BooleanVar(value=d.private),
            'government_employees': BooleanVar(value=d.government_employees),
            'fed_employees': BooleanVar(value=d.fed_employees),
            'state_employees': BooleanVar(value=d.state_employees),
            'local_employees': BooleanVar(value=d.local_employees),
            'needers_fully_participate': BooleanVar(value=d.needers_fully_participate),
            'random_seed': StringVar(value=g.random_seed),
            'self_employed': BooleanVar(value=d.self_employed),
            'state_of_work': BooleanVar(value=g.state_of_work),
            'top_off_rate': DoubleVar(value=d.top_off_rate),
            'top_off_min_length': IntVar(value=d.top_off_min_length),
            'weekly_ben_cap': IntVar(value=d.weekly_ben_cap),
            'weight_factor': IntVar(value=d.weight_factor),
            'eligible_earnings': IntVar(value=d.eligible_earnings),
            'eligible_weeks': IntVar(value=d.eligible_weeks),
            'eligible_hours': IntVar(value=d.eligible_hours),
            'eligible_size': IntVar(value=d.eligible_size),
            'payroll_tax': DoubleVar(value=d.payroll_tax),
            'benefits_tax': BooleanVar(value=d.benefits_tax),
            'average_state_tax': DoubleVar(value=d.average_state_tax),
            'max_taxable_earnings_per_person': IntVar(value=d.max_taxable_earnings_per_person),
            'total_taxable_earnings': IntVar(value=d.total_taxable_earnings),
            'counterfactual': StringVar(),
            'policy_sim': BooleanVar(value=d.policy_sim),
            'dual_receivers_share': DoubleVar(value=d.dual_receivers_share),
            'max_weeks': {leave_type: IntVar(value=d.max_weeks[leave_type]) for leave_type in LEAVE_TYPES},
            'take_up_rates': {leave_type: DoubleVar(value=d.take_up_rates[leave_type]) for leave_type in LEAVE_TYPES},
            'leave_probability_factors': {leave_type: DoubleVar(value=d.leave_probability_factors[leave_type])
                                          for leave_type in LEAVE_TYPES},
            'dependency_allowance': BooleanVar(value=d.dependency_allowance),
            'dependency_allowance_profile': [],
            'wait_period': IntVar(value=d.wait_period),
            'recollect': BooleanVar(value=d.recollect),
            'min_cfl_recollect': StringVar(value=d.min_cfl_recollect),
            'min_takeup_cpl': IntVar(value=d.min_takeup_cpl),
            'alpha': DoubleVar(value=d.alpha),
        }

        return variables

    def add_variable_callbacks(self):
        """Adds a callback when user changes certain inputs"""
        # When the file location entries are modified, check to see if they all have some value
        # If they do, enable the run button
        if sys.version_info[1] < 6:
            self.variables['fmla_file'].trace("w", self.check_file_entries)
            self.variables['acs_directory'].trace("w", self.check_file_entries)
            self.variables['output_directory'].trace("w", self.check_file_entries)
            self.variables['r_path'].trace("w", self.check_file_entries)

            # When users change the existing_program variable, change all parameters to match an existing state program
            self.variables['existing_program'].trace('w', self.set_existing_parameters)
            self.variables['state'].trace('w', self.save_config)
            self.variables['fmla_wave'].trace('w', self.save_config)
            self.variables['year'].trace('w', self.save_config)
        else:
            self.variables['fmla_file'].trace_add("write", self.check_file_entries)
            self.variables['acs_directory'].trace_add("write", self.check_file_entries)
            self.variables['output_directory'].trace_add("write", self.check_file_entries)
            self.variables['r_path'].trace_add("write", self.check_file_entries)
            self.variables['existing_program'].trace_add('write', self.set_existing_parameters)
            self.variables['state'].trace_add('write', self.save_config)
            self.variables['fmla_wave'].trace_add('write', self.save_config)
            self.variables['year'].trace_add('write', self.save_config)

    def on_close(self):
        """When the main window is closed, destroy all progress and result windows. Also destroy main window."""
        self.terminate_engine()
        for w in self.progress_windows:
            w.quit()
            w.destroy()
        for w in self.results_windows:
            w.quit()
            w.destroy()
        self.quit()
        self.destroy()

    def terminate_engine(self):
        if self.engine_process is not None and self.engine_process.is_alive():
            self.engine_process.terminate()

    def set_existing_parameters(self, *_):
        """Changes all relevant parameters to match an existing state program"""

        # Get the parameters for the state that the user has selected
        state = self.variables['existing_program'].get().upper()
        if state not in DEFAULT_STATE_PARAMS or state == '':
            return
        state_params = DEFAULT_STATE_PARAMS[state].copy()
        dependency_profile = state_params.pop('dependency_allowance_profile')
        self.update_dependency_allowance_profile(dependency_profile)

        for param_key, param_val in state_params.items():
            # If value for the parameter is a dictionary, then traverse that dictionary
            if type(param_val) == dict:
                for k, v in param_val.items():
                    self.variables[param_key][k].set(v)
            else:
                self.variables[param_key].set(param_val)

    def check_all_states(self):
        if not self.check_all_states_employees():
            return False

        params = self.all_params[0]
        main_eligibility = [params.private, params.self_employed, params.fed_employees, params.state_employees,
                            params.local_employees]
        if self.current_state == 'All' and main_eligibility != [False, False, True, True, True]:
            return messagebox.askokcancel('Warning', 'Running the model on all states will take a very long time and '
                                                     'could exceed the system\'s memory. Would you like to proceed?')

        return True

    def check_all_states_employees(self):
        if self.general_params.state != 'All' or not self.comparing:
            return True

        params = self.all_params[0]
        main_eligibility = [params.private, params.self_employed, params.fed_employees, params.state_employees,
                            params.local_employees]
        for i in range(1, len(self.all_params)):
            params = self.all_params[i]
            eligibility = [params.private, params.self_employed, params.fed_employees, params.state_employees,
                           params.local_employees]
            if main_eligibility != eligibility:
                messagebox.showinfo('Error', message='When comparing programs for All States, Eligible Employee Types '
                                                     'must be the same across all programs')
                return False

        return True

    def run_simulation(self):
        """Run the simulation from the parameters that user provides"""
        # Before running simulation, check for input errors
        self.clear_errors()
        errors = self.validate_inputs()

        # If there are errors, don't run simulation. Instead, display the errors.
        if len(errors) > 0:
            self.display_errors(errors)
            return

        # Save user input values to the setting objects
        self.save_general_parameters()
        self.save_params()
        self.current_state = self.general_params.state

        if not self.check_all_states():
            return

        self.currently_running = True
        self.run_button.disable()  # Prevent user from running another simulation when one is already running

        # Run either to Python or R engine
        if self.general_params.engine_type == 'Python':
            self.run_simulation_python()
        elif self.general_params.engine_type == 'R':
            self.run_simulation_r()

    def run_simulation_python(self):
        """Run Python engine"""
        q = multiprocessing.Queue()  # This is a messaging queue to get updates from engine

        # Initiate a SimulationEngine instance
        self.sim_engine = self.create_simulation_engine(q)

        # If comparing, add parameters for all of the programs to the engine
        for parameter in self.all_params:
            self.add_engine_params(parameter)
            if not self.comparing:
                break

        # Run model
        self.engine_process = multiprocessing.Process(None, target=run_engine_python, args=(self.sim_engine, q))
        self.engine_process.start()

        progress_window = ProgressWindow(self, self.engine_process)  # Create progress window
        self.progress_windows.append(progress_window)  # Keep track of all progress windows

        progress_window.update_progress_python(q)  # Update progress window with messages in queue

    def run_simulation_r(self):
        """Run R Engine"""
        # Create progress text file to update progress window
        progress_file = './r_model_full/progress/progress_{}.txt'.format(datetime.datetime.now().strftime('%Y%m%d%H%M%S%f'))
        if not os.path.exists('./r_model_full/progress'):
            os.makedirs('./r_model_full/progress')
        open(progress_file, 'w+').close()

        # Generate command to run R engine from terminal
        command = create_r_command(self.general_params, self.all_params[0], progress_file)
        print(command)

        # Run command in new process
        self.engine_process = multiprocessing.Process(None, target=run_engine_r, args=(command,))
        self.engine_process.start()

        # Display progress window and update with current engine progress
        progress_window = ProgressWindow(self, self.engine_process)
        self.progress_windows.append(progress_window)  # Keep track of all progress windows
        f = open(progress_file, 'r')
        progress_window.update_progress_r(f)

    def show_results(self, engine='Python'):
        """Display the results of the simulation"""
        self.engine_process.terminate()  # End the process that ran engine
        self.currently_running = False

        # Compute program costs
        print('Showing results')
        if engine == 'Python':
            costs = self.sim_engine.get_cost_df(0)
            main_output_dir = self.sim_engine.output_directories[0]
            results_files = self.sim_engine.get_results_files()
        else:
            results_files = [os.path.join(self.general_params.output_directory, 'output_py_compatible.csv')]
            costs = pd.read_csv(os.path.join(self.general_params.output_directory, 'program_cost.csv'))
            costs.columns = ['type', 'cost', 'ci_lower', 'ci_upper']
            main_output_dir = self.general_params.output_directory

        # Calculate total benefits paid
        total_benefits = list(costs.loc[costs['type'] == 'total', 'cost'])[0]

        # Create instance of ABF module with simulation results and user parameters
        main_params = self.all_params[0]
        abf_module = ABF(results_files[0], total_benefits, main_params.eligible_size,
                         main_params.max_taxable_earnings_per_person, main_params.benefits_tax,
                         main_params.average_state_tax, main_params.payroll_tax, output_dir=main_output_dir)

        # Keep track of all results windows that are created
        self.results_windows.append(ResultsWindow(self, costs, self.current_state, results_files, abf_module))
        self.run_button.enable()  # Enable run button again after simulation is complete

    def create_params(self):
        """Create an object to store the non-general parameter values"""
        # The inputs are linked to a tkinter variable. Those values will have to be retrieved from each variable
        # and passed on to the parameter objects.
        variable_values = {}
        valid_var_names = vars(self.default_params).keys()  # Only use non-general parameters
        for var_name, var_obj in self.variables.items():
            if var_name not in valid_var_names:
                continue

            if type(var_obj) == dict:
                # Some parameters should return a dictionary
                variable_values[var_name] = {k: v.get() for k, v in var_obj.items()}
            elif var_name == 'min_cfl_recollect':
                # Set this parameter to None if the field is empty
                try:
                    variable_values[var_name] = int(var_obj.get())
                except ValueError:
                    variable_values[var_name] = None
            elif var_name == 'dependency_allowance_profile':
                variable_values[var_name] = self.parameter_notebook.program_frame.dep_allowance_frame.get_profile()
            else:
                variable_values[var_name] = var_obj.get()

        return OtherParameters(**variable_values)

    def start_comparing(self):
        """Start comparing multiple programs"""
        self.comparing = True

    def stop_comparing(self):
        """Stop comparing programs"""
        self.comparing = False
        self.switch_comparison(0)  # Populate fields with main program parameter values

    def add_comparison(self):
        """Add a new program to compare"""
        self.all_params.append(self.default_params)

    def remove_comparison(self, sim_num):
        """Remove a program from comparisons

        :param sim_num: int, required
            The index of a comparison program to remove
        :return: None
        """
        del self.all_params[sim_num]
        self.switch_comparison(sim_num - 1, save=False)

    def switch_comparison(self, sim_num, save=True):
        """Switch to a different comparison program

        :param sim_num: int, required
            The index of a comparison program to switch to
        :param save: bool, default True
            Whether or not to save the current input values
        :return: None
        """
        # First, save the changes that users have made to to current program
        if save:
            self.save_params()
        self.parameter_notebook.simulation_frame.existing_program_input.current(0)
        # Switch to the new program
        self.current_sim_num = sim_num
        self.change_comparison_parameters(sim_num)

    def change_comparison_parameters(self, sim_num):
        """Update the inputs in the interface with the values of a chosen program

        :param sim_num: int, required
            The index of a comparison program
        :return: None
        """
        params = self.all_params[sim_num]  # Get the input values from parameter list and sim_num
        for param_key, param_val in vars(params).items():
            # If value for the parameter is a dictionary, then traverse that dictionary
            if type(param_val) == dict:
                for k, v in param_val.items():
                    self.variables[param_key][k].set(v)
            elif param_key == 'dependency_allowance_profile':
                self.variables[param_key] = param_val
                self.update_dependency_allowance_profile(param_val)
            else:
                self.variables[param_key].set(param_val)

    def save_general_parameters(self):
        """Update general parameters object with user input values"""

        # Store user-provided values in a dictionary
        variable_values = {}
        for var_name in vars(self.general_params).keys():
            variable_values[var_name] = self.variables[var_name].get()
        variable_values['random_seed'] = self.check_random_seed(variable_values['random_seed'])

        # Update the GeneralParameters object with dictionary
        self.general_params.update_variables(**variable_values)

    def save_params(self):
        """Save the non=general parameters for the current comparison program

        :return: OtherParameters()
        """
        params = self.create_params()
        self.all_params[self.current_sim_num] = params
        return params

    def update_dependency_allowance_profile(self, profile):
        """Update dependency allowance frame to create profile widgets based on profile list

        :param profile: list, required
            Dependency allowance profile. Can be empty.
        :return: None
        """

        self.parameter_notebook.program_frame.dep_allowance_frame.remove_all_dependents()
        self.parameter_notebook.program_frame.dep_allowance_frame.add_dependents(profile)

    @staticmethod
    def check_random_seed(random_seed):
        """Converts string seed to an integer

        :param random_seed: int or str
        :return: random_seed value converted to an integer
        """
        if random_seed is None or random_seed == '':
            return None

        try:
            return int(random_seed)
        except ValueError:
            return int.from_bytes(random_seed.encode(), 'big')

    def create_simulation_engine(self, q):
        """Create Python engine using parameter values

        :param q: multiprocessing.Queue()
            Will be passed to engine to receive progress updates in interface
        :return: SimulationEngine
        """

        # Get values from GeneralParameters object
        st = self.general_params.state.lower()
        yr = self.general_params.year
        fmla_wave = self.general_params.fmla_wave
        fp_fmla_in = self.general_params.fmla_file
        fp_cps_in = './data/cps/cps_clean_%s.csv' % (yr - 2)
        fp_acsh_in = self.general_params.acs_directory + '/%s/household_files' % yr
        fp_acsp_in = self.general_params.acs_directory + '/%s/person_files' % yr
        state_of_work = self.general_params.state_of_work
        if state_of_work:
            fp_acsh_in = self.general_params.acs_directory + '/%s/pow_household_files' % yr
            fp_acsp_in = self.general_params.acs_directory + '/%s/pow_person_files' % yr
        fp_fmla_out = './data/fmla/fmla_%s/fmla_clean_%s.csv' % (fmla_wave, fmla_wave)
        fp_dir_out = self.general_params.output_directory
        fp_cps_out = './data/cps/cps_for_acs_sim.csv'
        fp_acs_out = self.general_params.acs_directory
        fp_length_distribution_out = './data/fmla/fmla_%s/length_distributions_exact_days.json' % fmla_wave
        fps_in = [fp_fmla_in, fp_cps_in, fp_acsh_in, fp_acsp_in]
        fps_out = [fp_dir_out, fp_fmla_out, fp_cps_out, fp_acs_out, fp_length_distribution_out]

        clf_name = self.general_params.simulation_method
        random_seed = self.general_params.random_seed

        return SimulationEngine(st, yr, fmla_wave, fps_in, fps_out, clf_name=clf_name, random_state=random_seed,
                                state_of_work=state_of_work, q=q)

    def add_engine_params(self, parameters):
        """Add additional engine parameters from an OtherParameters object

        :param parameters: OtherParameters
        :return: None
        """

        # Get parameter values from OtherParameters object
        elig_wage12 = parameters.eligible_earnings
        elig_wkswork = parameters.eligible_weeks
        elig_yrhours = parameters.eligible_hours
        elig_empsize = parameters.eligible_size
        rrp = parameters.replacement_ratio
        wkbene_cap = parameters.weekly_ben_cap

        d_maxwk = {
            'own': parameters.max_weeks['Own Health'],
            'matdis': parameters.max_weeks['Maternity'],
            'bond': parameters.max_weeks['New Child'],
            'illchild': parameters.max_weeks['Ill Child'],
            'illspouse': parameters.max_weeks['Ill Spouse'],
            'illparent': parameters.max_weeks['Ill Parent']
        }

        d_takeup = {
            'own': parameters.take_up_rates['Own Health'],
            'matdis': parameters.take_up_rates['Maternity'],
            'bond': parameters.take_up_rates['New Child'],
            'illchild': parameters.take_up_rates['Ill Child'],
            'illspouse': parameters.take_up_rates['Ill Spouse'],
            'illparent': parameters.take_up_rates['Ill Parent']
        }

        incl_private = parameters.private
        incl_empgov_fed = parameters.fed_employees
        incl_empgov_st = parameters.state_employees
        incl_empgov_loc = parameters.local_employees
        incl_empself = parameters.self_employed
        needers_fully_participate = parameters.needers_fully_participate
        # state_of_work value see above next to fp_acsh_in/fp_acsp_in
        # weight_factor = parameters.weight_factor
        clone_factor = parameters.clone_factor
        dual_receivers_share = parameters.dual_receivers_share
        alpha = parameters.alpha
        min_takeup_cpl = parameters.min_takeup_cpl
        wait_period = parameters.wait_period
        recollect = parameters.recollect
        min_cfl_recollect = parameters.min_cfl_recollect
        dependency_allowance = parameters.dependency_allowance
        dependency_allowance_profile = parameters.dependency_allowance_profile
        leave_types = []
        if parameters.own_health:
            leave_types.append('own')
        if parameters.maternity:
            leave_types.append('matdis')
        if parameters.new_child:
            leave_types.append('bond')
        if parameters.ill_child:
            leave_types.append('illchild')
        if parameters.ill_spouse:
            leave_types.append('illspouse')
        if parameters.ill_parent:
            leave_types.append('illparent')

        # Update simulation engine with the values
        self.sim_engine.set_simulation_params(elig_wage12, elig_wkswork, elig_yrhours, elig_empsize, rrp, wkbene_cap,
                                              d_maxwk, d_takeup, incl_private, incl_empgov_fed, incl_empgov_st,
                                              incl_empgov_loc, incl_empself, needers_fully_participate, clone_factor,
                                              dual_receivers_share, alpha, min_takeup_cpl, wait_period,
                                              recollect, min_cfl_recollect, dependency_allowance,
                                              dependency_allowance_profile, leave_types=leave_types, sim_num=None)

    def check_file_entries(self, *_):
        """Enables run button if locations for ACS, FMLA, and output folders are provided. Otherwise, disables run
        button."""
        if self.variables['fmla_file'].get() and self.variables['acs_directory'].get() and \
                self.variables['output_directory'].get():

            if self.variables['engine_type'].get() == 'R':
                if self.variables['r_path'].get():
                    self.run_button.enable()
                else:
                    self.run_button.disable()
            else:
                self.run_button.enable()
        else:
            self.run_button.disable()

        if self.currently_running:
            self.run_button.disable()

        self.save_config()

    def validate_inputs(self):
        """Checks each entry value for correct data type and range.

        :return: List of errors
        """
        errors = []  # Keep track of errors

        # These are the inputs that are expecting integer values
        integer_entries = [
            self.parameter_notebook.program_frame.eligible_earnings_input,
            self.parameter_notebook.program_frame.eligible_weeks_input,
            self.parameter_notebook.program_frame.eligible_hours_input,
            self.parameter_notebook.program_frame.eligible_size_input,
            self.parameter_notebook.program_frame.weekly_ben_cap_input,
            self.parameter_notebook.simulation_frame.clone_factor_input,
            self.parameter_notebook.program_frame.benefit_financing_frame.max_taxable_earnings_per_person_input,
            self.parameter_notebook.program_frame.wait_period_input
        ]

        integer_entries += [entry for entry in self.parameter_notebook.program_frame.max_weeks_inputs]

        positive_int_entries = [self.parameter_notebook.population_frame.min_takeup_cpl_input]
        if self.variables['recollect'].get():
            positive_int_entries.append(self.parameter_notebook.program_frame.min_cfl_recollect_input)

        # These are the inputs that are expecting decimal values
        float_entries = [self.parameter_notebook.program_frame.benefit_financing_frame.payroll_tax_input,
                         self.parameter_notebook.program_frame.benefit_financing_frame.average_state_tax_input]

        # These are the inputs expecting decimal values between 0 and 1
        rate_entries = [self.parameter_notebook.program_frame.replacement_ratio_input,
                        self.parameter_notebook.population_frame.dual_receivers_share_input]
        rate_entries += [entry for entry in self.parameter_notebook.population_frame.take_up_rates_inputs]
        rate_entries += [p.input for p in self.parameter_notebook.program_frame.dep_allowance_frame.profiles]
        # rate_entries += [entry for entry in self.parameter_notebook.population_frame.leave_probability_factors_inputs]

        # Validate all of the inputs
        for entry in integer_entries:
            if not self.validate_integer(entry.get()):
                errors.append((entry, 'This field should contain an integer greater than or equal to 0'))

        for entry in float_entries:
            if not self.validate_positive_float(entry.get()):
                errors.append((entry, 'This field should contain a real number greater than or equal to 0'))

        for entry in rate_entries:
            if not self.validate_rate(entry.get()):
                errors.append((entry, 'This field should contain a number greater than or equal to '
                                      '0 and less than or equal to 1'))

        for entry in positive_int_entries:
            if not self.validate_positive_integer(entry.get()):
                errors.append((entry, 'This field should contain an integer greater than 0'))

        if not self.validate_float(self.parameter_notebook.population_frame.alpha_input.get()):
            errors.append((self.parameter_notebook.population_frame.alpha_input,
                           'This field should contain a real number'))

        errors += self.validate_file_inputs()
        errors += self.validate_leave_types()
        return errors

    def validate_file_inputs(self):
        errors = []
        file_inputs = [
            self.general_params_frame.fmla_input,
            self.general_params_frame.acs_input,
            self.general_params_frame.output_directory_input
        ]

        if self.variables['engine_type'] == 'R':
            file_inputs.append(self.general_params_frame.r_path_input)

        for entry in file_inputs:
            entry_val = entry.get()
            if not os.path.exists(entry_val):
                entry_val = os.path.expanduser(entry_val)
                if os.path.exists(entry_val):
                    entry.delete(0, END)  # Clear current value in entry widget
                    entry.insert(0, entry_val)  # Add expanded user filepath to entry widget
                else:
                    errors.append((entry, 'The file or directory does not exist'))

        return errors

    def validate_leave_types(self):
        errors = []
        if not (self.variables['own_health'].get() or self.variables['maternity'].get() or
                self.variables['new_child'].get() or self.variables['ill_child'].get() or
                self.variables['ill_spouse'].get() or self.variables['ill_parent'].get()):
            leave_types_frame = self.parameter_notebook.program_frame.leave_types_frame
            leave_type_inputs = [leave_types_frame.own_health_input,
                                 leave_types_frame.maternity_input,
                                 leave_types_frame.new_child_input,
                                 leave_types_frame.ill_child_input,
                                 leave_types_frame.ill_spouse_input,
                                 leave_types_frame.ill_parent_input]

            for entry in leave_type_inputs:
                errors.append((entry, 'At least one leave type needs to be selected.'))

        return errors

    @staticmethod
    def validate_integer(value):
        """Checks if value is an integer greater than 0

        :param value: integer or string
        :return: bool
        """
        try:
            return int(value) >= 0
        except ValueError:
            return False

    @staticmethod
    def validate_positive_integer(value):
        """Checks if value is an integer greater than 0

        :param value: integer or string
        :return: bool
        """
        try:
            return int(value) > 0
        except ValueError:
            return False

    @staticmethod
    def validate_float(value):
        """Checks if value is a float greater

        :param value: float or string
        :return: bool
        """
        try:
            return float(value) >= 0 or float(value) < 0
        except ValueError:
            return False

    @staticmethod
    def validate_positive_float(value):
        """Checks if value is a float greater than 0

        :param value: float or string
        :return: bool
        """
        try:
            return float(value) >= 0
        except ValueError:
            return False

    @staticmethod
    def validate_rate(value):
        """Checks if value is a float greater than 0 and less than 1

        :param value: float or string
        :return: bool
        """
        try:
            return 0 <= float(value) <= 1
        except ValueError:
            return False

    def display_errors(self, errors):
        """Visually displays error in main window

        :param errors: list of tuples
            Each tuple consists of a widget and an error message
        :return: None
        """
        for widget, error in errors:
            try:
                widget.config(bg='red', fg='white')  # Change color of input with invalid value to red
            except TclError:
                widget.config(style='MSCheckbuttonRed.TCheckbutton')
            # Add a tooltip to input to explain problem
            self.error_tooltips.append((widget, ToolTipCreator(widget, error)))

        # Create popup to alert user that one more more errors occured
        messagebox.showinfo('Error', message='There was an error with one or more entries.')

    def clear_errors(self):
        """Removes error visualizations from main window

        :return: None
        """
        for widget, tooltip in self.error_tooltips:
            try:
                widget.config(bg='white', fg='black')  # Turn background of entry widgets white
            except TclError:
                widget.config(style='MSCheckbutton.TCheckbutton')
            tooltip.hidetip()  # Remove any error tooltips created

        self.error_tooltips = []

    def scroll(self, event):
        """Allows the scroll wheel to move a scrollbar

        :param event: Event object
            Contains information about the scroll direction
        :return: None
        """
        # In Windows, the delta will be either 120 or -120. In Mac, it will be 1 or -1.
        # The delta value will determine whether the user is scrolling up or down.
        move_unit = 0
        if event.num == 5 or event.delta > 0:
            move_unit = -2
        elif event.num == 4 or event.delta < 0:
            move_unit = 2

        # Only scroll the tab that is currently visible.
        if self.current_tab == 0:
            self.parameter_notebook.program_frame.canvas.yview_scroll(move_unit, 'units')
        elif self.current_tab == 1:
            self.parameter_notebook.population_frame.canvas.yview_scroll(move_unit, 'units')
        elif self.current_tab == 2:
            self.parameter_notebook.simulation_frame.canvas.yview_scroll(move_unit, 'units')

    def change_current_tab(self, event):
        """Alerts program when the user clicks on a new tab

        :param event: Event object
            Contains information about which tab was clicked
        :return: None
        """
        self.current_tab = self.parameter_notebook.tk.call(self.parameter_notebook._w, "identify", "tab", event.x,
                                                           event.y)

    def hide_advanced_parameters(self):
        """Hides inputs that are for advanced users"""
        self.general_params_frame.hide_advanced_parameters()
        self.parameter_notebook.hide_advanced_parameters()
        self.update_idletasks()

    def show_advanced_parameters(self):
        """Reveals inputs that are for advanced users"""
        self.general_params_frame.show_advanced_parameters()
        self.parameter_notebook.show_advanced_parameters()
        self.update_idletasks()

    def toggle_advanced_parameters(self):
        """Switches between either showing or hiding advanced inputs"""
        # When more inputs are added to the main window, they will take up space. This causes certain widgets to
        # shrink or even disappear. So we need to increase the window size
        if self.showing_advanced:
            self.showing_advanced = False
            self.hide_advanced_parameters()
        else:
            self.showing_advanced = True
            self.show_advanced_parameters()

        # Change the color for the On and Off buttons in the Advanced Parameters frame
        self.advanced_frame.on_button.toggle()
        self.advanced_frame.off_button.toggle()
        self.save_config()

    def create_config(self):
        """Create a configuration file with default values"""

        # Set default values for configurations
        self.config['PATHS'] = {
            'fmla_file': '',
            'acs_directory': '',
            'output_directory': '',
            'r_path': ''
        }

        self.config['PREFERENCES'] = {
            'showing_advanced': 'False'
        }

        self.config['PARAMS'] = {
            'state': '',
            'engine_type': 'Python'
        }

        # Save configurations
        with open(self.config_fp, 'w') as f:
            self.config.write(f)

    def read_config(self):
        """Read a configuration file if it exists"""

        if not os.path.exists(self.config_fp):
            self.create_config()  # Create configuration file if it doesn't exist
        else:
            self.config.read(self.config_fp)  # Read configuration file

    def load_config(self):
        """Edit the parameter values based on values saved in the configurations file"""

        try:
            self.read_config()  # Read existing configuration file

            # Set path values
            for param, path in self.config['PATHS'].items():
                self.variables[param].set(path)

            # Set other parameter values
            for param, value in self.config['PARAMS'].items():
                self.variables[param].set(value)
        except Exception as e:
            # If there is an error with loading the configuration file, then just create a new one
            print('Error loading config.ini')
            print(type(e).__name__ + ':', e)
            self.create_config()

    def save_config(self, *_):
        """Save parameter values into a configurations file so that user doesn't have to reenter them next time they
        start the app"""

        # Create sections if they don't exist. Something had to have gone wrong to trigger this.
        sections = ['PATHS', 'PREFERENCES', 'PARAMS']
        for section in sections:
            if section not in self.config:
                self.config[section] = {}

        # Save paths
        paths_to_save = ['fmla_file', 'acs_directory', 'output_directory', 'r_path']
        for path in paths_to_save:
            self.config['PATHS'][path] = self.variables[path].get()

        # Save other parameters
        params_to_save = ['state', 'year', 'fmla_wave', 'engine_type']
        for param in params_to_save:
            self.config['PARAMS'][param] = str(self.variables[param].get())

        # Save whether or not advanced parameters are showing
        self.config['PREFERENCES']['showing_advanced'] = str(self.showing_advanced)

        # Save configurations file
        with open(self.config_fp, 'w') as f:
            self.config.write(f)


class GeneralParamsFrame(Frame):
    def __init__(self, parent=None, **kwargs):
        """Create a frame to hold general parameters"""
        super().__init__(parent, **kwargs)

        # Valid file types which will be used to restrict input files
        self.spreadsheet_ftypes = [('All', '*.xlsx; *.xls; *.csv'), ('Excel', '*.xlsx'),
                                   ('Excel 97-2003', '*.xls'), ('CSV', '*.csv')]

        # State codes
        self.states = ('All', 'AK', 'AL', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'DC', 'FL', 'GA', 'HI', 'ID', 'IL',
                       'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV',
                       'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN',
                       'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY')

        # Currently implemented simulation methods
        self.simulation_methods = ('Logistic Regression GLM', 'Logistic Regression Regularized', 'Ridge Classifier',
                                   'K Nearest Neighbor', 'Naive Bayes', 'Support Vector Machine',
                                   'Random Forest', 'XGBoost')
        self.cwd = os.getcwd()  # Current working directory
        self.variables = self.winfo_toplevel().variables

        # Create the input widgets for general parameters
        # ------------------------------------------------ FMLA File ------------------------------------------------
        tip = 'A CSV or Excel file that contains leave taking data to use to train model. ' \
              'This should be FMLA survey data.'
        self.fmla_label = TipLabel(self, tip, text="FMLA File:", bg=DARK_COLOR, fg=LIGHT_COLOR, anchor=N)
        self.fmla_input = GeneralEntry(self, textvariable=self.variables['fmla_file'])
        self.fmla_button = BorderButton(self, text="Browse",
                                        command=lambda: self.browse_file(self.fmla_input, self.spreadsheet_ftypes))
        self.fmla_button.config(width=None)

        # ------------------------------------------------ FMLA Wave ------------------------------------------------
        tip = "The year of the survey wave that the above FMLA file is for."
        fmla_waves = (2012, 2018)
        self.fmla_wave_label = TipLabel(self, tip, text="FMLA Wave:", bg=DARK_COLOR, fg=LIGHT_COLOR, anchor=N)
        self.fmla_wave_input = ttk.Combobox(self, textvariable=self.variables['fmla_wave'], state="readonly",
                                            width=7, values=fmla_waves)
        self.fmla_wave_input.current(fmla_waves.index(self.variables['fmla_wave'].get()))

        # ------------------------------------------------ ACS File -------------------------------------------------
        tip = 'A directory that contains ACS files that the model will use to estimate the cost of a paid ' \
              'leave program. There should be one household and one person file for the selected state.'
        self.acs_label = TipLabel(self, tip, text="ACS Directory:", bg=DARK_COLOR, fg=LIGHT_COLOR)
        self.acs_input = GeneralEntry(self, textvariable=self.variables['acs_directory'])
        self.acs_button = BorderButton(self, text="Browse",
                                       command=lambda: self.browse_directory(self.acs_input))

        # -------------------------------------------- Output Directory ---------------------------------------------
        tip = 'The directory where the spreadsheet containing simulation results will be saved.'
        self.output_directory_label = TipLabel(self, tip, text="Output Directory:", bg=DARK_COLOR, fg=LIGHT_COLOR)
        self.output_directory_input = GeneralEntry(self, textvariable=self.variables['output_directory'])
        self.output_directory_button = BorderButton(self, text="Browse",
                                                    command=lambda: self.browse_directory(self.output_directory_input))

        # ---------------------------------------------- Output Detail ----------------------------------------------
        # tip = 'The level of detail of the results. \n1 = low detail \n8 = high detail'
        # self.detail_label = TipLabel(self, tip, text="Output Detail Level:", bg=DARK_COLOR, fg=LIGHT_COLOR)
        # self.detail_input = ttk.Combobox(self, textvariable=self.variables['detail'], state="readonly", width=5,
        #                                  style='MSCombobox.TCombobox')
        # self.detail_input['values'] = (1, 2, 3, 4, 5, 6, 7, 8)
        # self.detail_input.current(0)

        # -------------------------------------------- State to Simulate --------------------------------------------
        tip = 'The state that will be used to estimate program cost. Only people  living or working in ' \
              'this state will be chosen from the input and  output files.'
        self.state_label = TipLabel(self, tip, text='State to Simulate:', bg=DARK_COLOR, fg=LIGHT_COLOR)
        self.state_input = ttk.Combobox(self, textvariable=self.variables['state'], state="readonly", width=7,
                                        values=self.states)

        # Set current value for state combobox
        try:
            self.state_input.current(self.states.index(self.variables['state'].get()))
        except (TclError, ValueError):
            self.state_input.current(0)

        # ---------------------------------------------- State of Work ----------------------------------------------
        tip = 'Whether or not the analysis is to be done for persons who work in particular state – ' \
              'rather than for residents of the state.'
        self.state_of_work_input = TipCheckButton(self, tip, text="State of Work",
                                                  style='DarkCheckbutton.TCheckbutton',
                                                  variable=self.variables['state_of_work'])

        # ------------------------------------------------ ACS Year -------------------------------------------------
        tip = 'Year of ACS data.'
        years = (2016, 2017, 2018)
        self.year_label = TipLabel(self, tip, text="Year:", bg=DARK_COLOR, fg=LIGHT_COLOR)
        self.year_input = ttk.Combobox(self, textvariable=self.variables['year'], state="readonly",
                                       width=7, values=years)
        self.year_input.current(years.index(self.variables['year'].get()))

        # -------------------------------------------- Simulation Method --------------------------------------------
        tip = 'The method used to train model.'
        self.simulation_method_label = TipLabel(self, tip, text='Simulation Method:', bg=DARK_COLOR,
                                                fg=LIGHT_COLOR)
        self.simulation_method_input = ttk.Combobox(self, textvariable=self.variables['simulation_method'],
                                                    state="readonly", width=30, values=self.simulation_methods)
        self.simulation_method_input.current(0)

        # ----------------------------------------------- Random Seed -----------------------------------------------
        tip = 'The value that will be used in random number generation. Can be used to recreate results as long ' \
              'as all other parameters are unchanged.'
        self.random_seed_label = TipLabel(self, tip, text="Random Seed:", bg=DARK_COLOR, fg=LIGHT_COLOR)
        self.random_seed_input = GeneralEntry(self, textvariable=self.variables['random_seed'], width=32)

        # ----------------------------------------------- Engine Type -----------------------------------------------
        tip = 'Choose between the Python and R model.'
        self.engine_type_label = TipLabel(self, tip, text='Engine Type:', bg=DARK_COLOR, fg=LIGHT_COLOR)
        self.engine_type_input = ttk.Combobox(self, textvariable=self.variables['engine_type'], state="readonly",
                                              width=7, values=['Python', 'R'])

        # Set current value of engine type combobox
        if self.variables['engine_type'].get() == 'Python':
            self.engine_type_input.current(0)
        else:
            self.engine_type_input.current(1)

        # ------------------------------------------------- R Path --------------------------------------------------
        tip = 'The Rscript path on your system.'
        self.r_path_label = TipLabel(self, tip, text="Rscript Path:", bg=DARK_COLOR, fg=LIGHT_COLOR)
        self.r_path_input = GeneralEntry(self, textvariable=self.variables['r_path'])
        self.r_path_button = BorderButton(
            self, text="Browse", command=lambda: self.browse_file(self.r_path_input, [('Rscript', 'Rscript.exe')]))
        self.variables['engine_type'].trace('w', self.toggle_r_path)

        # Add the input widgets to the parent widget
        self.row_padding = 2
        self.fmla_label.grid(column=0, row=0, sticky=W, pady=self.row_padding)
        self.fmla_input.grid(column=1, row=0, columnspan=3, sticky=(E, W), padx=8, pady=self.row_padding)
        self.fmla_button.grid(column=4, row=0, pady=self.row_padding)
        self.fmla_wave_label.grid(column=0, row=1, sticky=W, pady=self.row_padding)
        self.fmla_wave_input.grid(column=1, row=1, sticky=W, padx=8, pady=self.row_padding)
        self.acs_label.grid(column=0, row=2, sticky=W, pady=self.row_padding)
        self.acs_input.grid(column=1, row=2, columnspan=3, sticky=(E, W), padx=8, pady=self.row_padding)
        self.acs_button.grid(column=4, row=2, pady=self.row_padding)
        self.output_directory_label.grid(column=0, row=3, sticky=W, pady=self.row_padding)
        self.output_directory_input.grid(column=1, row=3, columnspan=3, sticky=(E, W), padx=8, pady=self.row_padding)
        self.output_directory_button.grid(column=4, row=3, pady=self.row_padding)
        self.state_label.grid(column=0, row=4, sticky=W, pady=(self.row_padding, 0))
        self.state_input.grid(column=1, row=4, sticky=W, padx=8, pady=(self.row_padding, 0))

        # Give the second column more space than the others
        self.columnconfigure(1, weight=1)

    def hide_advanced_parameters(self):
        """Hides inputs that are for advanced users"""
        # self.detail_label.grid_forget()
        # self.detail_input.grid_forget()
        self.state_of_work_input.grid_forget()
        self.year_label.grid_forget()
        self.year_input.grid_forget()
        self.simulation_method_label.grid_forget()
        self.simulation_method_input.grid_forget()
        self.random_seed_label.grid_forget()
        self.random_seed_input.grid_forget()
        self.engine_type_label.grid_forget()
        self.engine_type_input.grid_forget()
        self.r_path_label.grid_forget()
        self.r_path_input.grid_forget()
        self.r_path_button.grid_forget()

    def show_advanced_parameters(self):
        """Reveals inputs that are for advanced users"""
        # self.detail_label.grid(column=0, row=3, sticky=W, pady=self.row_padding)
        # self.detail_input.grid(column=1, row=3, sticky=W, padx=8, pady=self.row_padding)
        self.state_of_work_input.grid(column=1, row=6, columnspan=2, sticky=W, padx=8, pady=(0, self.row_padding))
        self.year_label.grid(column=0, row=7, sticky=W, pady=self.row_padding)
        self.year_input.grid(column=1, row=7, sticky=W, padx=8, pady=self.row_padding)
        self.simulation_method_label.grid(column=0, row=9, sticky=W, pady=self.row_padding)
        self.simulation_method_input.grid(column=1, row=9, sticky=W, padx=8, pady=self.row_padding)
        self.random_seed_label.grid(column=0, row=10, sticky=W, pady=self.row_padding)
        self.random_seed_input.grid(column=1, row=10, sticky=W, padx=8, pady=self.row_padding)
        self.engine_type_label.grid(column=0, row=11, sticky=W, pady=self.row_padding)
        self.engine_type_input.grid(column=1, row=11, sticky=W, padx=8, pady=self.row_padding)
        self.toggle_r_path()

    def browse_file(self, file_input, filetypes):
        """Open a file dialogue where user can choose a file. Possible options are limited to CSV and Excel files

        :param file_input: Entry widget that will hold the name of the file that user chooses
        :param filetypes: List of tuples, required
            Each tuple contains the name of a file category and the file extensions in that category
        :return: None
        """

        # Open the file dialog
        file_name = filedialog.askopenfilename(initialdir=self.cwd, filetypes=filetypes)

        # Don't replace the file input value if user cancels
        if file_name is not None and file_name != '':
            file_input.delete(0, END)  # Clear current value in entry widget
            file_input.insert(0, file_name)  # Add user-selected value to entry widget

    def browse_directory(self, directory_input):
        """Open a file dialogue where user can choose a directory.

        :param directory_input: Entry widget that will hold the name of the directory that user chooses
        :return: None
        """

        # Open the file dialogue
        directory_name = filedialog.askdirectory(initialdir=self.cwd)

        # Don't replace the directory input value if user cancels
        if directory_name is not None and directory_name != '':
            directory_input.delete(0, END)  # Clear current value in entry widget
            directory_input.insert(0, directory_name)  # Add user-selected value to entry widget

    def toggle_r_path(self, *_):
        """Either reveals or hides the widgets related to specifying path of Rscript.exe"""
        if self.variables['engine_type'].get() == 'R':
            self.r_path_label.grid(column=0, row=13, sticky=W, pady=self.row_padding)
            self.r_path_input.grid(column=1, row=13, padx=8, sticky=(E, W), pady=self.row_padding)
            self.r_path_button.grid(column=4, row=13, pady=self.row_padding)
        else:
            self.r_path_label.grid_forget()
            self.r_path_input.grid_forget()
            self.r_path_button.grid_forget()

        self.winfo_toplevel().check_file_entries()


class ComparisonFrame(Frame):
    def __init__(self, parent=None, **kwargs):
        """Frame to hold buttons to control program comparison functionalities"""
        super().__init__(parent, **kwargs)

        self.showing_comparisons = False  # Indicator for whether or not the program is currently in comparison mode
        self.comparison_count = 0  # Number of programs that are user is comparing
        self.comparison_max = 4  # Max number of programs allowed

        # Frame to hold buttons to start and stop comparison mode as well as to add additional programs to compare
        self.buttons = Frame(self, bg=DARK_COLOR)
        self.buttons.pack(side=RIGHT, anchor=E, fill=Y)

        # Button to start comparison mode
        self.start_button = BorderButton(self.buttons, custom=True, borderwidth=1, relief='flat',
                                         highlightbackground='#FFFFFF')
        button_content = SubtleToggle(self.start_button, text='Compare', font='-size 9 -weight bold',
                                      command=self.toggle_show_comparison)
        self.start_button.add_content(button_content)
        self.start_button.pack(side=RIGHT)

        # Button to add one comparison
        self.add_simulation_button = BorderButton(self.buttons, text=u'\uFF0B', font='-size 10 -weight bold',
                                                  background=THEME_COLOR, width=0, padx=2, pady=0,
                                                  highlightthickness=0, command=self.add_simulation)

        #  Frame to hold comparison proram buttons
        self.simulations_frame = Frame(self, bg=DARK_COLOR)
        self.main_sim = SimulationSelectFrame(self, parent=self.simulations_frame)
        self.main_sim.toggle_on()
        self.main_sim.pack(side=LEFT, anchor=E, padx=(0, 3))

        # List of comparison program buttons
        self.simulations = [self.main_sim]

    def toggle_show_comparison(self):
        """Starts or stops comparison mode depending on the current state"""
        if not self.showing_comparisons:
            # Show the buttons related to comparison mode functionality
            self.simulations_frame.pack(side=RIGHT, anchor=E, fill=BOTH)
            self.add_simulation_button.pack(side=LEFT, padx=(0, 6))
            self.start_button.button.toggle()
            self.showing_comparisons = True
            self.winfo_toplevel().start_comparing()  # Inform main window that comparison mode has started
        else:
            # Hide the buttons related to comparison mode functionality
            self.simulations_frame.pack_forget()
            self.add_simulation_button.pack_forget()
            self.start_button.button.toggle()
            self.showing_comparisons = False
            self.winfo_toplevel().stop_comparing()  # Inform main window that comparison mode has stopped

    def add_simulation(self):
        """Add a program to compare"""

        # Do not allow user to add more than the max number of comparison programs
        if self.comparison_count >= self.comparison_max:
            return

        self.comparison_count += 1  # Increment the count of comparisons

        # Create a new button for the new comparison program
        new_sim = SimulationSelectFrame(self, parent=self.simulations_frame, sim_num=self.comparison_count)
        new_sim.pack(side=LEFT, anchor=E, padx=(0, 3))
        self.simulations.append(new_sim)  # Add new program to list of comparisons
        self.winfo_toplevel().add_comparison()  # Add new program's parameters to the list of parameters

    def select_simulation(self, sim_num):
        """Set a comparison simulation as the currently selected one"""

        # Toggle all programs buttons off except for the one that is selected
        for i in range(len(self.simulations)):
            if i != sim_num:
                self.simulations[i].toggle_off()
            else:
                self.simulations[i].toggle_on()

    def remove_simulation(self, sim_num):
        """Removes the currently selected program from the set of comparison programs"""

        # Since the current program is being removed, switch to the previous program in the ordered list
        self.select_simulation(sim_num - 1)

        # Decrement the number of each program after the one that has just been removed
        for i in range(sim_num + 1, self.comparison_count + 1):
            self.simulations[i].update_sim_num(i - 1)

        # Remove current program button from window
        self.simulations[sim_num].pack_forget()
        del self.simulations[sim_num]
        self.comparison_count -= 1  # Decrement the count of simulations


class SimulationSelectFrame(Frame):
    def __init__(self, comparison_frame, parent=None, sim_num=0, width=None, **kwargs):
        """A frame that holds the button for a comparison program as well as a button to remove the program"""

        self.comparison_frame = comparison_frame  # Need to keep track of this frame when removing a program
        self.non_selected_color = '#808080'  # Color when button is not selected
        self.selected_color = '#FFFFFF'  # Color when button is selected
        super().__init__(parent, relief='flat', highlightbackground=self.non_selected_color, highlightthickness=1,
                         **kwargs)

        self.sim_num = sim_num  # Comparison program number - used to determine its name
        self.name = get_sim_name(sim_num)

        # Button to select the program
        self.select_button = SubtleButton(self, text=self.name, fg=self.non_selected_color, width=width,
                                          command=self.select)
        self.select_button.pack(side=LEFT)

        # Button to remove the program
        self.remove_button = SubtleButton(self, text=u'\u2A09', bg='#d9d9d9', fg='#666666', width=2,
                                          command=self.remove)

    def toggle_off(self):
        """Visually show that this program is not selected"""
        self.config(highlightbackground=self.non_selected_color)
        self.select_button.config(fg=self.non_selected_color)
        self.remove_button.pack_forget()  # Remove option to remove this program

    def toggle_on(self):
        """Visually show that this program is selected"""
        self.config(highlightbackground=self.selected_color)
        self.select_button.config(fg=self.selected_color)
        if self.sim_num > 0:
            self.remove_button.pack(side=LEFT)  # Show option to remove this program if it's not the main simulation

    def select(self):
        """Select this program"""
        self.comparison_frame.select_simulation(self.sim_num)
        self.winfo_toplevel().switch_comparison(self.sim_num)

    def remove(self):
        """Remove this program"""
        self.comparison_frame.remove_simulation(self.sim_num)
        self.winfo_toplevel().remove_comparison(self.sim_num)

    def update_sim_num(self, sim_num):
        """Change this program's number and update its name"""
        self.sim_num = sim_num

        # Change the button's text
        self.name = 'Comparison {}'.format(self.sim_num)
        self.select_button.config(text=self.name)


class ParameterNotebook(ttk.Notebook):
    def __init__(self, parent=None, **kwargs):
        """Notebook that holds all of the program parameter types"""
        super().__init__(parent, style='MSNotebook.TNotebook', **kwargs)

        # Create frames for each notebook tab. Each frame needs a canvas because scroll bars cannot be added to a frame.
        # They can only be added to canvases and listboxes. So another frame needs to be added inside the canvas. This
        # frame will contain the actual user input widgets.
        self.program_frame = ProgramFrame(self)
        self.population_frame = PopulationFrame(self)
        self.simulation_frame = SimulationFrame(self)

        # Add the frames to the notebook
        self.add(self.program_frame, text='Program')
        self.add(self.population_frame, text='Population')
        self.add(self.simulation_frame, text='Simulation')

        # In order to control scrolling in the right notebook tab, we need to keep track of the tab that
        # is currently visible. Whenever a tab is clicked, update this value.
        self.bind('<Button-1>', self.change_current_tab)
        # When the top window gets resized, the scroll regions in the notebook should be resized too
        self.bind('<Configure>', self.resize)

        self.update_idletasks()
        # Set the width of notebook to fit the content of the the content of all of its frames
        self.set_notebook_width(self.program_frame.content.winfo_width())
        self.config(width=self.program_frame.content.winfo_width() + 18)

    def change_current_tab(self, event):
        """Change the currently visible tab."""
        self.winfo_toplevel().current_tab = self.tk.call(self._w, "identify", "tab", event.x, event.y)

    def set_scroll_region(self, height=-1):
        """Changes the space that the vertical scrollbar can move through

        :param height: int
            Height to set the parameter frames in the notebook. If it is negative, then use the tallest frame's height
            as the height
        :return: None
        """

        scroll_frames = [self.program_frame, self.population_frame, self.simulation_frame]
        # If height is negative, use program_frame's canvas's height since it is the tallest
        canvas_height = self.program_frame.canvas.winfo_height() if height < 0 else height

        for frame in scroll_frames:
            frame_height = frame.content.winfo_height()

            # Update canvas to the new height if it is not larger than the frame height
            new_height = frame_height if frame_height > canvas_height else canvas_height
            frame.canvas.configure(scrollregion=(0, 0, 0, new_height))

    def set_notebook_width(self, width):
        """Changes the width of notebook's frames

        :param width: int, width to set each frame
        :return:
        """
        self.program_frame.canvas.itemconfig(1, width=width)
        self.population_frame.canvas.itemconfig(1, width=width)
        self.simulation_frame.canvas.itemconfig(1, width=width)

    def resize(self, event):
        """Is called when main window is resized to change the height and width of the notebook's frames"""
        new_width = event.width - 30
        self.set_notebook_width(new_width)
        self.set_scroll_region(event.height - 30)

    def hide_advanced_parameters(self):
        """Hides inputs that are for advanced users"""
        self.program_frame.hide_advanced_parameters()
        self.population_frame.hide_advanced_parameters()
        self.simulation_frame.hide_advanced_parameters()

    def show_advanced_parameters(self):
        """Reveals inputs that are for advanced users"""
        self.program_frame.show_advanced_parameters()
        self.population_frame.show_advanced_parameters()
        self.simulation_frame.show_advanced_parameters()


class ScrollFrame(Frame):
    def __init__(self, parent=None, content_bg=VERY_LIGHT_COLOR, **kwargs):
        """A frame with a vertical scroll bar that allows scrolling through the frame's content"""
        super().__init__(parent, **kwargs)

        # Create scroll bar
        self.scroll_bar = ttk.Scrollbar(self, orient=VERTICAL)
        self.scroll_bar.pack(side=RIGHT, fill=Y)

        # Canvas needed to hold scroll wheel and content frame
        self.canvas = Canvas(self, bg=VERY_LIGHT_COLOR, borderwidth=0, highlightthickness=0,
                             yscrollcommand=self.scroll_bar.set)
        self.canvas.pack(side=LEFT, fill=BOTH, expand=True, padx=0, pady=0)

        self.content = Frame(self, padx=10, pady=10, bg=content_bg, width=600)  # Frame holds the actual content
        self.canvas.create_window((0, 0), window=self.content, anchor='nw')  # Add frame to canvas
        self.scroll_bar.config(command=self.canvas.yview)

    def update_scroll_region(self):
        """Sets the scroll region to the height of the frame's content"""
        self.update_idletasks()
        self.canvas.configure(scrollregion=(0, 0, 0, self.content.winfo_height()))


class NotebookFrame(ScrollFrame):
    __metaclass__ = ABCMeta

    def __init__(self, parent=None, **kwargs):
        super().__init__(parent, **kwargs)
        self.default_params = self.winfo_toplevel().default_params
        self.row_padding = 2

    @abstractmethod
    def hide_advanced_parameters(self):
        """Hides inputs for advanced users"""
        raise NotImplementedError

    @abstractmethod
    def show_advanced_parameters(self):
        """Reveals inputs for advanced users"""
        raise NotImplementedError

    @staticmethod
    def create_leave_objects(parent, leave_vars):
        """Some inputs require an entry value for each leave type. It is better to store each input in a list than to
        create separate variables for all of them.

        :param parent: widget
        :param leave_vars: dict of Tk variables,
            Requires one variable for each leave type
        :return: list of label widgets and input widgets
        """
        leave_type_labels = []  # A list of label widgets for inputs
        leave_type_inputs = []  # A list of entry inputs
        for i, leave_type in enumerate(LEAVE_TYPES):
            # Create the label and entry widgets
            leave_type_labels.append(Label(parent, text=leave_type, bg=VERY_LIGHT_COLOR, font='-size 10'))
            leave_type_inputs.append(NotebookEntry(parent, textvariable=leave_vars[leave_type], justify='center',
                                                   width=10))
            parent.columnconfigure(i, weight=1)

        return leave_type_labels, leave_type_inputs


class ProgramFrame(NotebookFrame):
    def __init__(self, parent=None, **kwargs):
        """Frame to hold inputs related to Program parameters"""

        super().__init__(parent, **kwargs)
        self.variables = self.winfo_toplevel().variables
        v = self.variables

        # Create the input widgets for program parameters
        # ------------------------------------------- Program Eligibility -------------------------------------------
        # Inputs related to eligibility will be grouped in a label frame
        self.eligibility_frame_label = ttk.Label(self.content, text='Eligibility Rules:', cursor='question_arrow',
                                                 style='MSLabelframe.TLabelframe.Label', font='-size 10')
        self.eligibility_frame = ttk.Labelframe(self.content, labelwidget=self.eligibility_frame_label,
                                                style='MSLabelframe.TLabelframe')
        ToolTipCreator(self.eligibility_frame_label, 'The requirements to be eligible for the paid leave program.')

        # Earnings
        tip = 'The amount of money earned in the last year.'
        self.eligible_earnings_label = TipLabel(self.eligibility_frame, tip, text="Earnings", bg=VERY_LIGHT_COLOR,
                                                font='-size 10')
        self.eligible_earnings_input = NotebookEntry(self.eligibility_frame, textvariable=v['eligible_earnings'],
                                                     justify='center', width=15)

        # Weeks worked
        tip = 'The number of weeks worked in the last year.'
        self.eligible_weeks_label = TipLabel(self.eligibility_frame, tip, text="Weeks", bg=VERY_LIGHT_COLOR,
                                             font='-size 10')
        self.eligible_weeks_input = NotebookEntry(self.eligibility_frame, textvariable=v['eligible_weeks'],
                                                  justify='center', width=15)

        # Hours worked
        tip = 'The number of hours worked in the last year.'
        self.eligible_hours_label = TipLabel(self.eligibility_frame, tip, text="Hours", bg=VERY_LIGHT_COLOR,
                                             font='-size 10')
        self.eligible_hours_input = NotebookEntry(self.eligibility_frame, textvariable=v['eligible_hours'],
                                                  justify='center', width=15)

        # Employer size
        tip = 'Size of the employer.'
        self.eligible_size_label = TipLabel(self.eligibility_frame, tip, text="Employer Size", bg=VERY_LIGHT_COLOR,
                                            font='-size 10')
        self.eligible_size_input = NotebookEntry(self.eligibility_frame, textvariable=v['eligible_size'],
                                                 justify='center', width=15)

        # ----------------------------------------- Max Weeks with Benefits -----------------------------------------
        self.max_weeks_frame_label = ttk.Label(self.content, text='Max Weeks:', style='MSLabelframe.TLabelframe.Label',
                                               cursor='question_arrow', font='-size 10')
        self.max_weeks_frame = ttk.Labelframe(self.content, labelwidget=self.max_weeks_frame_label,
                                              style='MSLabelframe.TLabelframe')
        self.max_weeks_labels, self.max_weeks_inputs = self.create_leave_objects(self.max_weeks_frame, v['max_weeks'])
        ToolTipCreator(self.max_weeks_frame_label,
                       'The maximum number of weeks for each leave type that the program will pay for.')

        # ----------------------------------------- Employee Types Allowed ------------------------------------------
        self.employee_types_label = ttk.Label(self.content, text='Eligible Employee Types:', cursor='question_arrow',
                                              style='MSLabelframe.TLabelframe.Label', font='-size 10')
        self.employee_types_frame = EmployeeTypesFrame(self.content, v, labelwidget=self.employee_types_label,
                                                       style='MSLabelframe.TLabelframe')
        ToolTipCreator(self.employee_types_label, 'The types of employees that will be eligible for program.')

        # ------------------------------------------- Leave Types Allowed -------------------------------------------
        self.leave_types_label = ttk.Label(self.content, text='Leave Types Allowed:', cursor='question_arrow',
                                           style='MSLabelframe.TLabelframe.Label', font='-size 10')
        self.leave_types_frame = LeaveTypesFrame(self.content, v, labelwidget=self.leave_types_label,
                                                 style='MSLabelframe.TLabelframe')
        ToolTipCreator(self.leave_types_label, 'The leave types that the program will provide benefits for.')

        # ----------------------------------------- Wage Replacement Ratio ------------------------------------------
        tip = 'The percentage of wage that the program will pay.'
        self.replacement_ratio_label = TipLabel(self.content, tip, text="Replacement Ratio:", bg=VERY_LIGHT_COLOR)
        self.replacement_ratio_input = NotebookEntry(self.content, textvariable=v['replacement_ratio'])

        # ------------------------------------------- Weekly Benefit Cap --------------------------------------------
        tip = 'The maximum amount of benefits paid out per week.'
        self.weekly_ben_cap_label = TipLabel(self.content, tip, text="Weekly Benefit Cap:", bg=VERY_LIGHT_COLOR)
        self.weekly_ben_cap_input = NotebookEntry(self.content, textvariable=v['weekly_ben_cap'])

        # -------------------------------------------- Benefit Financing --------------------------------------------
        self.benefit_financing_label = ttk.Label(self.content, text='Benefit Financing:', cursor='question_arrow',
                                                 style='MSLabelframe.TLabelframe.Label', font='-size 10')
        self.benefit_financing_frame = BenefitFinancingFrame(self.content, v, labelwidget=self.benefit_financing_label,
                                                             style='MSLabelframe.TLabelframe')
        ToolTipCreator(self.benefit_financing_label, 'Parameters related to program financing.')

        # ------------------------------------------ Dependency Allowance -------------------------------------------
        self.dep_allowance_frame = DependencyAllowanceFrame(self.content, self.variables)

        # ----------------------------------------------- Wait Period -----------------------------------------------
        tip = 'Check this box to enable additional wage replacement for eligible dependents of applicant.'
        self.wait_period_label = TipLabel(self.content, tip, text="Waiting Period:", bg=VERY_LIGHT_COLOR)
        self.wait_period_input = NotebookEntry(self.content, textvariable=v['wait_period'])

        # ------------------------------------------------ Recollect ------------------------------------------------
        tip = 'Check this box to enable recollection of benefits that were not distributed during waiting period.'
        self.recollect_input = TipCheckButton(self.content, tip, variable=v['recollect'], text='Recollect')

        # Minimum Leave Length Required for Recollection
        tip = 'Minimum leave length (in number work days) required for recollection.'
        self.min_cfl_recollect_label = TipLabel(self.content, tip, text="Minimum Leave Length:", bg=VERY_LIGHT_COLOR,
                                                font='-size 10 -weight bold')
        self.min_cfl_recollect_input = NotebookEntry(self.content, textvariable=v['min_cfl_recollect'], font='-size 10')

        # Reveal min_cfl_recollect widgets only if recollect is checked and hide if unchecked
        v['recollect'].trace("w", self.toggle_min_cfl_recollect)

        # Add input widgets to the parent widget
        self.eligibility_frame.grid(column=0, row=0, columnspan=10, sticky=(N, E, W), pady=self.row_padding)
        self.eligible_earnings_label.grid(column=0, row=0)
        self.eligible_weeks_label.grid(column=1, row=0)
        self.eligible_hours_label.grid(column=2, row=0)
        self.eligible_size_label.grid(column=3, row=0)
        self.eligible_earnings_input.grid(column=0, row=1, sticky=(E, W), padx=1, pady=(0, 2))
        self.eligible_weeks_input.grid(column=1, row=1, sticky=(E, W), padx=1, pady=(0, 2))
        self.eligible_hours_input.grid(column=2, row=1, sticky=(E, W), padx=1, pady=(0, 2))
        self.eligible_size_input.grid(column=3, row=1, sticky=(E, W), padx=1, pady=(0, 2))

        self.max_weeks_frame.grid(column=0, row=1, columnspan=10, sticky=(N, E, W), pady=self.row_padding)
        display_leave_objects(self.max_weeks_labels, self.max_weeks_inputs)
        self.employee_types_frame.grid(column=0, row=3, columnspan=10, sticky=(N, E, W), pady=self.row_padding)
        self.benefit_financing_frame.grid(column=0, row=4, columnspan=10, sticky=(N, E, W), pady=self.row_padding)
        self.replacement_ratio_label.grid(column=0, row=5, sticky=W, pady=self.row_padding)
        self.replacement_ratio_input.grid(column=1, row=5, sticky=W, pady=self.row_padding)
        self.weekly_ben_cap_label.grid(column=0, row=6, sticky=W, pady=self.row_padding)
        self.weekly_ben_cap_input.grid(column=1, row=6, sticky=W, pady=self.row_padding)

        # Give weight to columns
        self.columnconfigure(1, weight=1)
        for i in range(4):
            self.eligibility_frame.columnconfigure(i, weight=1)

    def hide_advanced_parameters(self):
        self.leave_types_frame.grid_forget()
        self.dep_allowance_frame.grid_forget()
        self.wait_period_label.grid_forget()
        self.wait_period_input.grid_forget()
        self.recollect_input.grid_forget()
        self.min_cfl_recollect_label.grid_forget()
        self.min_cfl_recollect_input.grid_forget()

    def show_advanced_parameters(self):
        self.leave_types_frame.grid(column=0, row=2, columnspan=10, sticky=(N, E, W), pady=self.row_padding)
        self.wait_period_label.grid(column=0, row=7, sticky=W, pady=self.row_padding)
        self.wait_period_input.grid(column=1, row=7, sticky=W, pady=self.row_padding)
        self.dep_allowance_frame.grid(column=0, row=8, columnspan=10, sticky=(N, E, W), pady=self.row_padding)
        self.toggle_min_cfl_recollect()

    def toggle_min_cfl_recollect(self, *_):
        if self.variables['recollect'].get():
            self.recollect_input.grid(column=0, row=9, sticky=W, pady=(self.row_padding, 0))
            self.min_cfl_recollect_label.grid(column=0, row=10, sticky=W, pady=(0, self.row_padding), padx=(15, 0))
            self.min_cfl_recollect_input.grid(column=1, row=10, sticky=W, pady=(0, self.row_padding))
        else:
            self.recollect_input.grid(column=0, row=9, sticky=W, pady=self.row_padding)
            self.min_cfl_recollect_label.grid_forget()
            self.min_cfl_recollect_input.grid_forget()
        self.update_scroll_region()


class PopulationFrame(NotebookFrame):
    def __init__(self, parent=None, **kwargs):
        """Frame to hold inputs related to Population parameters"""
        super().__init__(parent, **kwargs)
        self.variables = self.winfo_toplevel().variables

        # Create the input widgets for population parameters
        # ---------------------------------------------- Take Up Rates ----------------------------------------------
        self.take_up_rates_frame_label = ttk.Label(self.content, text='Take Up Rates:', cursor='question_arrow',
                                                   style='MSLabelframe.TLabelframe.Label', font='-size 10')
        self.take_up_rates_frame = ttk.Labelframe(self.content, labelwidget=self.take_up_rates_frame_label,
                                                  style='MSLabelframe.TLabelframe')
        self.take_up_rates_labels, self.take_up_rates_inputs = \
            self.create_leave_objects(self.take_up_rates_frame, self.variables['take_up_rates'])
        ToolTipCreator(self.take_up_rates_frame_label, 'The proportion of eligible leave takers who decide to use the '
                                                       'program for each leave type.')

        # ---------------------------------------- Leave Probability Factors ----------------------------------------
        self.leave_probability_factors_frame_label = ttk.Label(self.content, text='Leave Probability Factors:',
                                                               style='MSLabelframe.TLabelframe.Label',
                                                               cursor='question_arrow')
        self.leave_probability_factors_frame = ttk.Labelframe(self.content,
                                                              labelwidget=self.leave_probability_factors_frame_label,
                                                              style='MSLabelframe.TLabelframe')
        self.leave_probability_factors_labels, self.leave_probability_factors_inputs = \
            self.create_leave_objects(self.leave_probability_factors_frame, self.variables['leave_probability_factors'])
        ToolTipCreator(self.leave_probability_factors_frame_label, 'Factors the probability of needing or taking '
                                                                   'a leave for each type of leave.')

        # --------------------------------------------- Benefit Effect ----------------------------------------------
        tip = 'Whether or not the benefit amount affects participation in the program.'
        self.benefit_effect_input = TipCheckButton(self.content, tip, text="Benefit Effect",
                                                   variable=self.variables['benefit_effect'])

        # ---------------------------------------- Participants Extend Leave ----------------------------------------
        tip = 'Whether or not participants extend their leave in the presence of the program.'
        self.extend_input = TipCheckButton(self.content, tip, text="Extend", variable=self.variables['extend'])

        # ---------------------------------------- Needers Fully Participate ----------------------------------------
        tip = 'Whether or not all people who need leave take leave in the presnce of the program.'
        self.needers_fully_participate_input = TipCheckButton(self.content, tip, text="Needers Fully Participate",
                                                              variable=self.variables['needers_fully_participate'])

        # ---------------------------------------------- Top Off Rate -----------------------------------------------
        tip = 'The proportion of employers already paying full wages in the absence of the program ' \
              'that will top off benefits in the presence of a program to reach full wages.'
        self.top_off_rate_label = TipLabel(self.content, tip, text="Top Off Rate:", bg=VERY_LIGHT_COLOR)
        self.top_off_rate_input = NotebookEntry(self.content, textvariable=self.variables['top_off_rate'])

        # ----------------------------------------- Top Off Minimum Length ------------------------------------------
        tip = 'The number of days employers will top off benefits.'
        self.top_off_min_length_label = TipLabel(self.content, tip, text="Top Off Minimum Length:",
                                                 bg=VERY_LIGHT_COLOR)
        self.top_off_min_length_input = NotebookEntry(self.content, textvariable=self.variables['top_off_min_length'])

        # ----------------------------------------- Share of Dual Receivers -----------------------------------------
        tip = 'Dual receiver of company and state benefits'
        self.dual_receivers_share_label = TipLabel(self.content, tip, text='Share of Dual Receivers:',
                                                   bg=VERY_LIGHT_COLOR)
        self.dual_receivers_share_input = NotebookEntry(self.content,
                                                        textvariable=self.variables['dual_receivers_share'])

        # -------------------------------------- Minimum Leave Length Applied ---------------------------------------
        tip = 'Minimum leave length (in number of work days) applied for by applicants'
        self.min_takeup_cpl_label = TipLabel(self.content, tip, text='Minimum Leave Length Applied:',
                                             bg=VERY_LIGHT_COLOR)
        self.min_takeup_cpl_input = NotebookEntry(self.content, textvariable=self.variables['min_takeup_cpl'])

        # -------------------------------------------------- Alpha --------------------------------------------------
        tip = 'A hyper-parameter that governs the weighted random draw of program takers from ACS sample. Alpha = 0 ' \
              'characterizes an unweighted random draw. Alpha>0 characterizes random draws biased towards workers ' \
              'with longer leave needs.'
        self.alpha_label = TipLabel(self.content, tip, text='Alpha:', bg=VERY_LIGHT_COLOR)
        self.alpha_input = NotebookEntry(self.content, textvariable=self.variables['alpha'])

        # Add input widgets to the parent widget
        self.take_up_rates_frame.grid(column=0, row=0, columnspan=10, sticky=(N, E, W), pady=self.row_padding)
        display_leave_objects(self.take_up_rates_labels, self.take_up_rates_inputs)
        display_leave_objects(self.leave_probability_factors_labels, self.leave_probability_factors_inputs)
        # self.benefit_effect_input.grid(column=0, row=2, columnspan=2, sticky=W)
        # self.extend_input.grid(column=0, row=3, columnspan=3, sticky=W)
        self.dual_receivers_share_label.grid(column=0, row=4, sticky=W, pady=self.row_padding)
        self.dual_receivers_share_input.grid(column=1, row=4, sticky=W, pady=self.row_padding)

        # Make second column take up more space
        self.columnconfigure(1, weight=1)

    def hide_advanced_parameters(self):
        self.min_takeup_cpl_label.grid_forget()
        self.min_takeup_cpl_input.grid_forget()
        self.alpha_label.grid_forget()
        self.alpha_input.grid_forget()

    def show_advanced_parameters(self):
        self.min_takeup_cpl_label.grid(column=0, row=5, sticky=W, pady=self.row_padding)
        self.min_takeup_cpl_input.grid(column=1, row=5, sticky=W, pady=self.row_padding)
        self.alpha_label.grid(column=0, row=6, sticky=W, pady=self.row_padding)
        self.alpha_input.grid(column=1, row=6, sticky=W, pady=self.row_padding)


class SimulationFrame(NotebookFrame):
    def __init__(self, parent=None, **kwargs):
        """Frame to hold inputs related to Simulation parameters"""
        super().__init__(parent, **kwargs)
        self.variables = self.winfo_toplevel().variables
        v = self.variables

        # ----------------------------------------- Existing State Program ------------------------------------------
        tip = 'Sets program parameters to match an existing state program.'
        self.existing_program_label = TipLabel(self.content, tip, text='Existing State Program:', bg=VERY_LIGHT_COLOR)
        self.existing_program_input = ttk.Combobox(self.content, textvariable=v['existing_program'],
                                                   state="readonly", width=5, values=list(DEFAULT_STATE_PARAMS.keys()))
        self.existing_program_input.current(0)

        # Create the input widgets for simulation parameters
        # ---------------------------------------------- Clone Factor -----------------------------------------------
        tip = 'The number of times each sample person will be run through the simulation.'
        self.clone_factor_label = TipLabel(self.content, tip, text="Clone Factor:", bg=VERY_LIGHT_COLOR)
        self.clone_factor_input = NotebookEntry(self.content, textvariable=v['clone_factor'])

        # ----------------------------------------------- SE Analysis -----------------------------------------------
        tip = 'Whether or not weight should be divided by clone factor value.'
        self.se_analysis_input = TipCheckButton(self.content, tip, text="SE Analysis", variable=v['se_analysis'])

        # ---------------------------------------------- Weight Factor ----------------------------------------------
        # tip = 'Multiplies the sample weights by value.'
        # self.weight_factor_label = TipLabel(self.content, tip, text="Weight Factor:", bg=VERY_LIGHT_COLOR)
        # self.weight_factor_input = MSNotebookEntry(self.content, textvariable=v['weight_factor'])

        # --------------------------------------- FMLA Protection Constraint ----------------------------------------
        tip = 'If checked, leaves that are extended due to a paid leave program will be capped at 12 weeks.'
        self.fmla_protection_constraint_input = TipCheckButton(self.content, tip, text="FMLA Protection Constraint",
                                                               variable=v['fmla_protection_constraint'])

        # ------------------------------------------------ Calibrate ------------------------------------------------
        tip = '''Indicates whether or not the calibration add-factors are used in the equations giving the probability
        of taking or needing leaves. These calibration factors adjust the simulated probabilities of taking or needing
        the most recent leave to equal those in the Family and Medical Leave in 2012: Revised Public Use File
        Documentation (McGarry et al, Abt Associates, 2013).'''
        self.calibrate_input = TipCheckButton(self.content, tip, text="Calibrate", variable=v['calibrate'])

        # # ---------------------------------------- Compare Against Existing -----------------------------------------
        # tip = 'Simulate a counterfactual scenario to compare user parameters  against a real paid leave program.'
        # self.counterfactual_label = TipLabel(self.content, tip, text='Compare Against Existing:', bg=VERY_LIGHT_COLOR)
        # self.counterfactual_input = ttk.Combobox(self.content, textvariable=v['counterfactual'],
        #                                          state="readonly", width=5, values=list(DEFAULT_STATE_PARAMS.keys()))
        # self.counterfactual_input.current(0)
        #
        # # ---------------------------------------- Compare Against Generous -----------------------------------------
        # tip = 'Simulate a policy scenario to compare user parameters against a  generous paid leave program ' \
        #       'in which everyone is eligible and the wage replacement is 1.'
        # self.policy_sim_input = TipCheckButton(self.content, tip, text="Compare Against Generous",
        #                                        variable=v['policy_sim'])

        # Add input widgets to the parent widget
        self.existing_program_label.grid(column=0, row=0, sticky=W, pady=self.row_padding)
        self.existing_program_input.grid(column=1, row=0, sticky=W, pady=self.row_padding)
        # self.counterfactual_label.grid(column=0, row=0, sticky=W, pady=self.row_padding)
        # self.counterfactual_input.grid(column=1, row=0, sticky=W, pady=self.row_padding)
        # self.policy_sim_input.grid(column=0, row=1, columnspan=2, sticky=W, pady=self.row_padding)

        # self.se_analysis_input.grid(column=0, row=1, columnspan=2, sticky=W)
        # self.calibrate_input.grid(column=0, row=4, columnspan=2, sticky=W)

    def hide_advanced_parameters(self):
        # self.weight_factor_label.grid_forget()
        # self.weight_factor_input.grid_forget()
        self.clone_factor_label.grid_forget()
        self.clone_factor_input.grid_forget()
        # self.fmla_protection_constraint_input.grid_forget()

    def show_advanced_parameters(self):
        # self.weight_factor_label.grid(column=0, row=2, sticky=W, pady=self.row_padding)
        # self.weight_factor_input.grid(column=1, row=2, sticky=W, pady=self.row_padding)
        self.clone_factor_label.grid(column=0, row=2, sticky=W, pady=self.row_padding)
        self.clone_factor_input.grid(column=1, row=2, sticky=W, pady=self.row_padding)
        # self.fmla_protection_constraint_input.grid(column=0, row=3, columnspan=2, sticky=W, pady=self.row_padding)


class EmployeeTypesFrame(ttk.LabelFrame):
    def __init__(self, parent, variables, row_padding=2, **kwargs):
        """Frame to hold the type of employees eligible for the leave program"""
        super().__init__(parent, **kwargs)
        self.variables = variables
        # -------------------------------------------- Private Employees --------------------------------------------
        tip = 'Whether or not private employees are eligible for program.'
        self.private_input = TipCheckButton(self, tip, text="Private Employees", variable=variables['private'])

        # ------------------------------------ Government Employees Eligibility -------------------------------------
        # All Government Employees
        tip = 'Whether or not government employees are eligible for program.'
        self.government_employees_input = TipCheckButton(self, tip, text="Government Employees",
                                                         variable=variables['government_employees'],
                                                         command=self.check_all_gov_employees)

        # Federal Employees
        tip = 'Whether or not federal employees are eligible for program.'
        self.federal_employees_input = TipCheckButton(self, tip, text="Federal Employees",
                                                      variable=variables['fed_employees'],
                                                      command=self.check_gov_employees,
                                                      style='MSCheckbuttonSmall.TCheckbutton')

        # State Employees
        tip = 'Whether or not state employees are eligible for program.'
        self.state_employees_input = TipCheckButton(self, tip, text="State Employees",
                                                    variable=variables['state_employees'],
                                                    command=self.check_gov_employees,
                                                    style='MSCheckbuttonSmall.TCheckbutton')

        # Local Government Employees
        tip = 'Whether or not local employees are eligible for program.'
        self.local_employees_input = TipCheckButton(self, tip, text="Local Employees",
                                                    variable=variables['local_employees'],
                                                    command=self.check_gov_employees,
                                                    style='MSCheckbuttonSmall.TCheckbutton')

        # ------------------------------------ Self-Employed Worker Eligibility -------------------------------------
        tip = 'Whether or not self employed workers are eligible for program.'
        self.self_employed_input = TipCheckButton(self, tip, text="Self Employed", variable=variables['self_employed'])

        # Add input widgets to the parent widget
        self.private_input.pack(pady=(row_padding, 0), padx=(12, 0), anchor=W)
        self.self_employed_input.pack(pady=(row_padding, 0), padx=(12, 0), anchor=W)
        self.government_employees_input.pack(pady=(row_padding, 0), padx=(12, 0), anchor=W)
        self.federal_employees_input.pack(padx=(24, 0), anchor=W)
        self.state_employees_input.pack(padx=(24, 0), anchor=W)
        self.local_employees_input.pack(padx=(24, 0), pady=(0, row_padding), anchor=W)

    def check_all_gov_employees(self, _=None):
        """Sets federal, state, and local employee variables to the value to the government employee variable"""
        checked = self.variables['government_employees'].get()
        self.variables['fed_employees'].set(checked)
        self.variables['state_employees'].set(checked)
        self.variables['local_employees'].set(checked)

    def check_gov_employees(self):
        """Sets government employee variable to True if federal, state, and local employee variables are all True.
        Otherwise, sets it to False."""
        if self.variables['fed_employees'].get() and self.variables['state_employees'].get() and \
                self.variables['local_employees'].get():
            self.variables['government_employees'].set(1)
        else:
            self.variables['government_employees'].set(0)


class LeaveTypesFrame(ttk.LabelFrame):
    def __init__(self, parent, variables, row_padding=2, **kwargs):
        """Frame to hold inputs related to leave type parameters"""
        super().__init__(parent, **kwargs)

        # ----------------------------------------------- Own Health ------------------------------------------------
        tip = ''
        self.own_health_input = TipCheckButton(self, tip, text='Own Health', variable=variables['own_health'])
        self.own_health_input.grid(row=0, column=0, sticky=W, pady=row_padding, padx=(12, 15))

        # ------------------------------------------------ Maternity ------------------------------------------------
        tip = ''
        self.maternity_input = TipCheckButton(self, tip, text='Maternity', variable=variables['maternity'])
        self.maternity_input.grid(row=0, column=1, sticky=W, pady=row_padding, padx=15)

        # ------------------------------------------------ New Child ------------------------------------------------
        tip = ''
        self.new_child_input = TipCheckButton(self, tip, text='New Child', variable=variables['new_child'])
        self.new_child_input.grid(row=0, column=2, sticky=W, pady=row_padding, padx=15)

        # ------------------------------------------------ Ill Child ------------------------------------------------
        tip = ''
        self.ill_child_input = TipCheckButton(self, tip, text='Ill Child', variable=variables['ill_child'])
        self.ill_child_input.grid(row=1, column=0, sticky=W, pady=row_padding, padx=(12, 15))

        # ----------------------------------------------- Ill Spouse ------------------------------------------------
        tip = ''
        self.ill_spouse_input = TipCheckButton(self, tip, text='Ill Spouse', variable=variables['ill_spouse'])
        self.ill_spouse_input.grid(row=1, column=1, sticky=W, pady=row_padding, padx=15)

        # ----------------------------------------------- Ill Parent ------------------------------------------------
        tip = ''
        self.ill_parent_input = TipCheckButton(self, tip, text='Ill Parent', variable=variables['ill_parent'])
        self.ill_parent_input.grid(row=1, column=2, sticky=W, pady=row_padding, padx=15)


class BenefitFinancingFrame(ttk.LabelFrame):
    def __init__(self, parent, variables, row_padding=2, wraplength=0, **kwargs):
        """Frame to hold inputs related to Benefit financing parameters"""
        super().__init__(parent, **kwargs)

        # Tax on Payroll
        tip = 'The payroll tax rate that will be assessed to fund the benefits program.'
        self.payroll_tax_label = TipLabel(self, tip, text='Payroll Tax Rate (%):', bg=VERY_LIGHT_COLOR)
        self.payroll_tax_input = NotebookEntry(self, textvariable=variables['payroll_tax'])

        # Maximum Taxable Earnings per Person
        tip = 'The maximum income level that can be taxed. For example, if $100,000 is entered then only earnings up ' \
              'to $100,000 per person will be taxed.'
        self.max_taxable_earnings_per_person_label = TipLabel(self, tip, text='Maximum Taxable Earnings ($):',
                                                              bg=VERY_LIGHT_COLOR, wraplength=wraplength)
        self.max_taxable_earnings_per_person_input = NotebookEntry(
            self, textvariable=variables['max_taxable_earnings_per_person'])

        # Tax on Benefits
        tip = 'Check this box to recoup state income taxes from the benefits dollars that are disbursed.'
        self.benefits_tax_input = TipCheckButton(self, tip, text='Apply Benefits Tax',
                                                 variable=variables['benefits_tax'])

        # Average State Tax
        tip = 'The applicable income tax rate on benefits.'
        self.average_state_tax_label = TipLabel(self, tip, text='State Income Tax Rate (%):', bg=VERY_LIGHT_COLOR,
                                                font='-size 10 -weight bold')
        self.average_state_tax_input = NotebookEntry(self, textvariable=variables['average_state_tax'])

        # Add input widgets to the parent widget
        self.payroll_tax_label.grid(column=0, row=0, sticky=W, padx=(8, 0), pady=row_padding)
        self.payroll_tax_input.grid(column=1, row=0, sticky=W, pady=row_padding)
        self.max_taxable_earnings_per_person_label.grid(column=0, row=1, sticky=W, padx=(8, 0), pady=row_padding)
        self.max_taxable_earnings_per_person_input.grid(column=1, row=1, sticky=W, pady=row_padding)
        self.benefits_tax_input.grid(column=0, row=2, columnspan=2, sticky=W, padx=(8, 0), pady=(row_padding, 0))
        self.average_state_tax_label.grid(column=0, row=3, sticky=W, padx=(16, 0), pady=(0, row_padding))
        self.average_state_tax_input.grid(column=1, row=3, sticky=W, pady=(0, row_padding))


class DependencyAllowanceFrame(Frame):
    def __init__(self, parent, variables, bg=VERY_LIGHT_COLOR, **kwargs):
        """Frame to hold inputs related to dependency allowance"""

        super().__init__(parent, bg=bg, **kwargs)
        self.parent = parent
        self.variables = variables
        self.max_dependents = 7  # Max number of dependents that can be added to the profile

        # ------------------------------------------ Dependency Allowance -------------------------------------------
        tip = 'Check this box to enable additional wage replacement for eligible dependents of applicant.'
        self.dependency_allowance_input = TipCheckButton(self, tip, text='Dependency Allowance',
                                                         variable=variables['dependency_allowance'])
        self.dependency_allowance_input.pack(anchor=W)

        # If the dependency allowance box is checked, then the profile input will be shown
        variables['dependency_allowance'].trace('w', self.toggle_dependency_allowance_profile)

        # -------------------------------------- Dependency Allowance Profile ---------------------------------------
        self.profiles = []
        self.profile_frame = Frame(self, bg=VERY_LIGHT_COLOR)

        # Labels that describe the profile inputs
        self.labels_frame = Frame(self.profile_frame, bg=VERY_LIGHT_COLOR)
        self.labels_frame.grid(row=0, column=0, sticky=E, padx=2)
        Label(self.labels_frame, text='Dependents', bg=VERY_LIGHT_COLOR, font='-size 10 -weight bold').\
            pack(side=TOP, anchor=E, pady=2)
        Label(self.labels_frame, text='Replacement Ratio', bg=VERY_LIGHT_COLOR, font='-size 10 -weight bold').\
            pack(side=TOP, anchor=E, pady=2)

        # Frame to hold buttons to add or remove profile inputs
        self.buttons_frame = Frame(self.profile_frame, bg=VERY_LIGHT_COLOR)
        self.add_button = Button(self.buttons_frame, text=u'\uFF0B', font='-size 9 -weight bold', relief='flat',
                                 background='#00e600', width=3, padx=0, pady=0, highlightthickness=0,
                                 foreground='#FFFFFF', command=self.add_dependent)
        self.add_button.pack(side=TOP, pady=2)
        self.remove_button = Button(self.buttons_frame, text=u'\uFF0D', font='-size 9 -weight bold', relief='flat',
                                    background='#ff0000', width=3, padx=0, pady=0, highlightthickness=0,
                                    foreground='#FFFFFF', command=self.remove_dependent)
        self.remove_button.pack(side=TOP, pady=2)
        self.buttons_frame.grid(row=0, column=1, padx=2, sticky=E)

    def add_dependent(self):
        """Add a dependent level to the dependency allowance profile"""
        if len(self.profiles) >= self.max_dependents:  # Don't add if maces is reached
            return

        # Remove the '+' from the last profile level before adding new level. So '1+' becomes '1'
        if len(self.profiles) > 0:
            self.profiles[-1].remove_plus()

        # Create new dependency level and add to profile frame
        profile = DependencyAllowanceProfileFrame(self.profile_frame, len(self.profiles) + 1)
        self.profiles.append(profile)
        profile.grid(row=0, column=len(self.profiles), padx=2)
        self.move_buttons_frame()  # Move the add and remove buttons

    def remove_dependent(self):
        """Remove the last dependent level"""
        if len(self.profiles) > 0:
            removed_dependent = self.profiles.pop(-1)
            removed_dependent.destroy()
            self.move_buttons_frame()  # Move the add and remove buttons

        # Add a plus sign to the last dependent level label. So if it is '1', it becomes '1+'.
        if len(self.profiles) > 0:
            self.profiles[-1].add_plus()

    def add_dependents(self, profiles):
        """Add dependency levels based on profile list

        :param profiles: list, required
            Dependency allowance profile. Can be empty.
        :return:
        """
        for i in range(len(profiles)):
            if len(self.profiles) < self.max_dependents:
                self.add_dependent()
                self.profiles[-1].replacement_ratio.set(profiles[i])

    def remove_all_dependents(self):
        """Remove all dependency levels"""
        for i in range(len(self.profiles)):
            self.remove_dependent()

    def move_buttons_frame(self):
        """Move frame that holds add and remove buttons to the right"""
        self.buttons_frame.grid(row=0, column=len(self.profiles) + 1)

    def toggle_dependency_allowance_profile(self, *_):
        """Display or hide the dependency allowance profile frame depending on whether the dependency allowance input
        is checked"""
        if self.variables['dependency_allowance'].get():
            self.profile_frame.pack(fill=X, padx=(15, 0))
        else:
            self.profile_frame.pack_forget()

        # Need to update the scroll region whenever adding or removing widgets from a scroll frame
        self.parent.master.update_scroll_region()

    def get_profile(self):
        """Returns values from dependency allowance profile inputs as a list

        :return: list
        """
        return [x.replacement_ratio.get() for x in self.profiles]


class DependencyAllowanceProfileFrame(Frame):
    def __init__(self, parent, num, **kwargs):
        """Frame that holds information about a dependency profile level

        :param parent: Tk widget
        :param num: Number of dependents
        :param kwargs: Other widget options
        """

        super().__init__(parent, bg=VERY_LIGHT_COLOR, width=10, **kwargs)
        self.num = num
        self.replacement_ratio = DoubleVar(value=0.0)
        self.label = Label(self, text=str(num) + '+', bg=VERY_LIGHT_COLOR, font='-size 10 -weight bold')
        self.label.pack(side=TOP, pady=2)
        self.input = NotebookEntry(self, textvariable=self.replacement_ratio, width=5, font='-size 10',
                                   justify='center')
        self.input.pack(side=TOP, pady=2)

    def add_plus(self):
        """Add a plus sign to the dependency level label"""
        self.label.config(text=str(self.num) + '+')

    def remove_plus(self):
        """Remove the plus sign from the dependency level label"""
        self.label.config(text=str(self.num))


class ResultsWindow(Toplevel):
    def __init__(self, parent, costs, state, results_files, abf_module):
        """Window to display results

        :param parent: Parent widget
        :param costs: pd.DataFrame, required
            Benefits paid for each leave type, along with confidence intervals
        :param state: str, required
            State that the simulation was run on
        :param results_files: list of file paths, required
            File paths to the CSV results of main simulation as well as comparison simulations
        :param abf_module: ABF, required
            Instance of ABF() used to calculate program revenue
        """

        super().__init__(parent)

        self.withdraw()  # Hide window until all of the widgets have been created
        self.protocol("WM_DELETE_WINDOW", self.on_close)  # To be executed when window in closed

        # Attach icon to window
        self.icon = PhotoImage(file='impaq_logo.gif')
        self.tk.call('wm', 'iconphoto', self._w, self.icon)

        self.parent = parent
        self.content = Frame(self)

        # Notebook will be used to separate the different types of result visualizations
        self.notebook = ttk.Notebook(self.content, style='MSNotebook.TNotebook', height=600)
        self.notebook.bind('<Button-1>', self.change_current_tab)
        self.current_tab = 0

        # Summary tab will display the costs of the each leave type of the main simulation
        self.summary_frame = ResultsSummary(self, costs, state, content_bg=DARK_COLOR)
        self.notebook.add(self.summary_frame, text='Summary')

        # ABF tab wil display the revenue information and functionalities
        self.abf = ABFResults(self.notebook, abf_module, content_bg=DARK_COLOR)
        self.notebook.add(self.abf, text="Benefit Financing")

        # Population analysis tab will display the information and functionalities of leave taken
        self.population_analysis = PopulationAnalysis(self.notebook, results_files, content_bg=DARK_COLOR)
        self.notebook.add(self.population_analysis, text='Population Analysis')

        # Add the content and notebook frame to the window
        self.content.pack(expand=True, fill=BOTH)
        self.notebook.pack(expand=True, fill=BOTH)
        self.notebook.select(self.summary_frame)
        self.notebook.enable_traversal()  # Allow traversal of tabs using keyboard

        self.bind("<MouseWheel>", self.scroll)  # Enable scrolling with scroll wheel
        # self.bind('<Configure>', self.resize)

        # Set the notebook size to fit its contents
        self.update_idletasks()
        self.notebook.config(width=self.abf.content.winfo_width() + 18)
        self.summary_frame.update_scroll_region()
        self.abf.update_scroll_region()
        self.population_analysis.update_scroll_region()
        self.resizable(False, False)
        self.deiconify()  # Reveal window now that widgets have been created

    def scroll(self, event):
        """Scrolls window based on the mouse wheel event"""
        # In Windows, the delta will be either 120 or -120. In Mac, it will be 1 or -1.
        # The delta value will determine whether the user is scrolling up or down.
        move_unit = 0
        if event.num == 5 or event.delta > 0:
            move_unit = -2
        elif event.num == 4 or event.delta < 0:
            move_unit = 2

        # Only scroll the tab that is currently visible.
        if self.current_tab == 0:
            self.summary_frame.canvas.yview_scroll(move_unit, 'units')
        elif self.current_tab == 1:
            self.abf.canvas.yview_scroll(move_unit, 'units')
        elif self.current_tab == 2:
            self.population_analysis.canvas.yview_scroll(move_unit, 'units')

    def change_current_tab(self, event):
        """Set current tab to the one that user selects. Used for scrolling with mouse wheel."""
        self.current_tab = self.notebook.tk.call(self.notebook._w, "identify", "tab", event.x, event.y)

    def set_notebook_width(self, width):
        """Changes width of the notebook widget

        :param width: int, required
            Width to set notebook
        :return: None
        """
        self.abf.canvas.itemconfig(1, width=width)
        self.population_analysis.canvas.itemconfig(1, width=width)

    def on_close(self):
        """Destroys window on close"""
        self.destroy()


class PopulationAnalysis(ScrollFrame):
    def __init__(self, parent, results_files, **kwargs):
        """Frame holds widgets related to leave days taken based on population characteristics

        :param parent: Tk widget, required
        :param results_files: list of file paths, required
            File paths to the CSV results of main simulation as well as comparison simulations
        """

        super().__init__(parent, **kwargs)
        self.results_files = results_files

        # Frame to hold inputs for population characteristics
        self.parameters_frame = Frame(self.content, highlightcolor='white', highlightthickness=1, pady=6, padx=8,
                                      bg=DARK_COLOR)
        self.parameters_frame.pack(padx=10, pady=10)

        # Gender input
        self.gender = StringVar()
        self.gender_label = Label(self.parameters_frame, text='Gender:', font='Helvetica 12 bold', bg=DARK_COLOR,
                                  fg=LIGHT_COLOR)
        self.gender_input = ttk.Combobox(self.parameters_frame, textvariable=self.gender, state="readonly", width=10,
                                         values=['Both', 'Male', 'Female'])
        self.gender_input.current(0)

        # Age input (max and min)
        self.age_min = IntVar()
        self.age_max = IntVar()
        self.age_label = Label(self.parameters_frame, text='Age:', font='Helvetica 12 bold', bg=DARK_COLOR,
                               fg=LIGHT_COLOR)
        self.age_min_label = Label(self.parameters_frame, text='Min', bg=DARK_COLOR, fg=LIGHT_COLOR)
        self.age_max_label = Label(self.parameters_frame, text='Max', bg=DARK_COLOR, fg=LIGHT_COLOR)
        self.age_min_input = GeneralEntry(self.parameters_frame, textvariable=self.age_min, width=10)
        self.age_max_input = GeneralEntry(self.parameters_frame, textvariable=self.age_max, width=10)

        # Wage input (max and min)
        self.wage_min = DoubleVar()
        self.wage_max = DoubleVar()
        self.wage_label = Label(self.parameters_frame, text='Wage:', font='Helvetica 12 bold', bg=DARK_COLOR,
                                fg=LIGHT_COLOR)
        self.wage_min_label = Label(self.parameters_frame, text='Min', bg=DARK_COLOR, fg=LIGHT_COLOR)
        self.wage_max_label = Label(self.parameters_frame, text='Max', bg=DARK_COLOR, fg=LIGHT_COLOR)
        self.wage_min_input = GeneralEntry(self.parameters_frame, textvariable=self.wage_min, width=18)
        self.wage_max_input = GeneralEntry(self.parameters_frame, textvariable=self.wage_max, width=18)

        # Parameter submission button
        self.submit_button = BorderButton(self.parameters_frame, text='Submit',
                                          command=lambda: self.update_histograms())

        # Add all widgets to the frame
        self.gender_label.grid(column=0, row=1, sticky=W, pady=2)
        self.gender_input.grid(column=0, row=2, sticky=W, pady=2)
        self.age_label.grid(column=1, row=0, sticky=W, pady=2)
        self.age_min_label.grid(column=1, row=1, sticky=W, pady=2)
        self.age_max_label.grid(column=2, row=1, sticky=W, pady=2)
        self.age_min_input.grid(column=1, row=2, sticky=W, padx=2)
        self.age_max_input.grid(column=2, row=2, sticky=W, padx=2)
        self.wage_label.grid(column=3, row=0, sticky=W, pady=2)
        self.wage_min_label.grid(column=3, row=1, sticky=W, pady=2)
        self.wage_max_label.grid(column=4, row=1, sticky=W, pady=2)
        self.wage_min_input.grid(column=3, row=2, sticky=W, padx=2)
        self.wage_max_input.grid(column=4, row=2, sticky=W, padx=2)
        self.submit_button.grid(column=0, row=3, sticky=W, pady=4)

        # Histogram properties
        self.bin_size = 5  # Bin size is one work week
        self.max_weekdays = 262  # This is the maximum number of work days in a year
        self.bins = list(range(0, self.max_weekdays, self.bin_size))
        self.xticks = list(range(0, self.max_weekdays, 20))

        # Frame to hold histograms
        self.histogram_frame = Frame(self.content, bg=DARK_COLOR)
        self.histograms = []
        self.histogram_frame.pack(side=TOP, fill=BOTH, expand=True)

        # Upon first initialization, add histograms to the frame
        self.create_histograms()

    def create_histograms(self):
        """Create new histogram charts for each simulation"""
        for sim_num in range(len(self.results_files)):
            # Get data from results files and user inputs
            leaves, weights = self.get_population_analysis_results(self.results_files[sim_num])
            title = get_sim_name(sim_num)  # Get title from simulation number

            # Create histogram from data
            histogram = self.create_histogram(leaves, self.bins, title, weights=weights, xticks=self.xticks)
            self.histograms.append(histogram)  # Add histogram to list of histograms
            chart_container = ChartContainer(self.histogram_frame, histogram, DARK_COLOR)
            chart_container.pack()

    def update_histograms(self):
        """Update histogram charts for each simulation"""
        for sim_num in range(len(self.results_files)):
            # Get data from results files and user inputs
            leaves, weights = self.get_population_analysis_results(self.results_files[sim_num])

            # Find created histogram chart in list
            fig = self.histograms[sim_num]
            ax = fig.axes[0]
            ax.cla()

            # Change histogram data
            ax.hist(leaves, self.bins, weights=weights, color='#1aff8c', rwidth=0.9)
            # Set the chart's style back to original
            self.set_histogram_properties(fig, ax, get_sim_name(sim_num), xticks=self.xticks)
            fig.canvas.draw()
            fig.canvas.flush_events()

    def get_population_analysis_results(self, output_fp, types=None, chunksize=100000):
        """Get the number of leave days per person from simulation results

        :param output_fp: str, required
            Name of simulated individual results
        :param types: list of str, default None
            Each element in list is a leave type
        :param chunksize: int, default 100000
            Number of rows to load into memory at once
        :return: (list of leave days, list of weights)
        """
        # Read in simulated acs, this is just df returned from get_acs_simulated()
        if types is None:
            types = ['own', 'matdis', 'bond', 'illchild', 'illspouse', 'illparent']

        usecols = ['PWGTP', 'female', 'age', 'wage12', 'nochildren', 'asian', 'black', 'white', 'native', 'other',
                   'hisp'] + ['takeup_%s' % t for t in types] + ['cpl_%s' % t for t in types]

        leaves = []
        weights = []

        for df in pd.read_csv(output_fp, usecols=lambda c: c in set(usecols), chunksize=chunksize):
            # Restrict to workers who take up the program
            types = [t for t in types if 'takeup_%s' % t in df.columns]
            df['takeup_any'] = df[['takeup_%s' % t for t in types]].sum(axis=1) > 0
            df = df[df['takeup_any']]

            # Make sure cpl_type is non-missing
            for t in types:
                df['cpl_%s' % t] = df['cpl_%s' % t].fillna(0)

            # Total covered-by-program length
            df['cpl'] = [sum(x) for x in df[['cpl_%s' % t for t in types]].values]
            # Keep needed vars for population analysis plots
            keepcols = ['PWGTP', 'cpl', 'female', 'age', 'wage12', 'nochildren', 'asian', 'black', 'white', 'native',
                        'other', 'hisp']
            df = df[keepcols]
            df = self.filter_data(df)

            leaves += df['cpl'].tolist()
            weights += df['PWGTP'].tolist()

        return leaves, weights

    def filter_data(self, data):
        """Filter out data and keep rows based on user population characteristic inputs

        :param data: pd.DataFrame, required
            Individual results data from simulation
        :return: Filtered dataframe
        """

        # Filter based on gender input
        if self.gender.get() == 'Male':
            data = data[data['female'] == 0]
        elif self.gender.get() == 'Female':
            data = data[data['female'] == 1]

        # Filter based on maximum and minimum wage inputs
        if self.age_min.get() is not None:
            data = data[data['age'] >= self.age_min.get()]
        if self.age_max.get() is not None and self.age_max.get() > 0:
            data = data[data['age'] <= self.age_max.get()]

        # Filter based on maximum and minimum wage inputs
        if self.wage_min.get() is not None:
            data = data[data['wage12'] >= self.wage_min.get()]
        if self.wage_max.get() is not None and self.wage_max.get() > 0:
            data = data[data['wage12'] <= self.wage_max.get()]

        return data

    def create_histogram(self, data, bins, title_str, weights=None, xticks=None):
        """Create histogram for leave days taken

        :param data: pd.DataFrame, required
            Individual results data from simulation
        :param bins: int or sequence or str, required
            Bins used for histogram. See matplotlib.pyplot.hist
        :param title_str: str, required
            Title of chart
        :param weights: array_like or None, optional
            Weights used for histogram. See matplotlib.pyplot.hist
        :param xticks: list or None, optional
            List of x-axis tick locations
        :return: instance of matplotlib.figure.Figure
        """

        # Create matplotlib figure
        fig = Figure(figsize=(8, 4))
        ax = fig.add_subplot(111)

        # Create histogram from data, bins, and weights
        ax.hist(data, bins, weights=weights, color='#1aff8c', rwidth=0.9)
        self.set_histogram_properties(fig, ax, title_str, xticks=xticks)  # Set various chart properties
        return fig

    def set_histogram_properties(self, fig, ax, title_str, xticks=None):
        """Set histogram title, axis labels, x axis ticks, and various styles

        :param fig: matplotlib.figure.Figure, required
        :param ax:  matplotlib.axes.Axes, required
        :param title_str: str, required
            Title of chart
        :param xticks:  list or None, optional
            List of x-axis tick locations
        :return: None
        """

        # Set x and y axis labels
        ax.set_ylabel('Number of Workers', fontsize=9)
        ax.set_xlabel('Number of Days', fontsize=9)

        # Set axis ticks
        if xticks is not None:
            ax.set_xticks(xticks)

        # Create axis title
        title = 'State: {}. Leaves Taken under Program. {}'.format(self.winfo_toplevel().parent.general_params.state,
                                                                   title_str)
        format_chart(fig, ax, title, DARK_COLOR, 'white')  # Change chart's style


class ResultsSummary(ScrollFrame):
    def __init__(self, parent, costs, state, **kwargs):
        """Frame to hold summary results from main simulation

        :param parent: Tk widget, required
        :param costs: pd.DataFrame, required
            Benefits paid for each leave type, along with confidence intervals
        :param state: string, required
            State that was analyzed in the simulation
        """

        super().__init__(parent, **kwargs)
        self.frame = Frame(self.content, bg=DARK_COLOR)
        self.frame.pack(fill=BOTH, expand=True)

        # Create summary chart
        cost_chart = create_cost_chart(costs, state)
        self.cost_chart_container = ChartContainer(self.frame, cost_chart, DARK_COLOR, height=400)
        self.cost_chart_container.pack()

        # Create leave chart
        leave_chart = create_leave_chart(costs, state)
        self.leave_chart_container = ChartContainer(self.frame, leave_chart, DARK_COLOR, height=400)
        self.leave_chart_container.pack()

        # # Create canvas for summary chart
        # canvas = FigureCanvasTkAgg(self.chart, self.chart_container)
        # canvas.draw()
        # canvas.get_tk_widget().pack(side=TOP, fill=BOTH, expand=True)
        # self.chart_container.pack(fill=X, padx=15, pady=15)
        #
        # # Create button to save chart
        # save_button = BorderButton(self.chart_container, text='Save Figure', width=10, pady=1,
        #                            command=lambda: self.save_file())
        # save_button.pack(side=RIGHT, padx=10, pady=10)

    # def save_file(self):
    #     """Saves matplotlib chart"""
    #     filename = filedialog.asksaveasfilename(
    #         defaultextension='.png', initialdir=os.getcwd(),
    #         filetypes=[('PNG', '.png'), ('PDF', '*.pdf'), ('PGF', '*.pgf'), ('EPS', '*.eps'), ('PS', '*.ps'),
    #                    ('Raw', '*.raw'), ('RGBA', '*.rgba'), ('SVG', '*.svg'), ('SVGZ', '*.svgz')]
    #     )
    #
    #     # Don't save if user cancels
    #     if filename is None or filename == '':
    #         return
    #     self.chart.savefig(filename, facecolor='#333333', edgecolor='white')


class ChartContainer(Frame):
    def __init__(self, parent, chart, bg_color, height=300):
        """Container that displays a matplotlib chart

        :param parent: Tk widget, required
        :param chart: matplotlib.figure.Figure, required
            Chart created using matplotlib
        :param bg_color: str, required
            Background color of chart
        """

        super().__init__(parent, bg=bg_color)
        self.chart = chart
        self.bg_color = bg_color

        # Create canvas for to hold chart
        self.canvas = FigureCanvasTkAgg(chart, self)
        self.canvas.draw()
        self.canvas.get_tk_widget().config(height=height)
        self.canvas.get_tk_widget().pack(side=TOP, fill=X)

        # Create button to save chart
        save_button = BorderButton(self, text='Save Figure', width=10, command=lambda: self.save_file())
        save_button.pack(side=LEFT, padx=10, pady=4)

    def save_file(self):
        """Saves matplotlib chart"""
        filename = filedialog.asksaveasfilename(
            defaultextension='.png', initialdir=os.getcwd(),
            filetypes=[('PNG', '.png'), ('PDF', '*.pdf'), ('PGF', '*.pgf'), ('EPS', '*.eps'), ('PS', '*.ps'),
                       ('Raw', '*.raw'), ('RGBA', '*.rgba'), ('SVG', '*.svg'), ('SVGZ', '*.svgz')]
        )

        # Don't save if user cancels
        if filename is None or filename == '':
            return
        self.chart.savefig(filename, facecolor=self.bg_color, edgecolor='white')


class ABFResults(ScrollFrame):
    def __init__(self, parent, abf_module, **kwargs):
        """Frame to hold results of ABF module

        :param parent: Tk widget, required
        :param abf_module: ABF, required
            Instance of ABF() used to calculate program revenue
        :param kwargs: Other widget options
        """
        super().__init__(parent, **kwargs)

        # Get ABF results from module
        self.abf_module = abf_module
        abf_output, pivot_tables = self.abf_module.run()
        # abf_module.save_results()  # Save ABF results

        # Create frame to hold summary of ABF results
        self.abf_summary = ABFResultsSummary(self.content, abf_output)
        self.abf_summary.pack(padx=10, pady=10)

        # Create frame to hold pivot tables for ABF results
        self.abf_pivot_tables = Frame(self.content, bg=DARK_COLOR)
        self.abf_pivot_tables.pack(fill=BOTH, expand=True)
        self.display_abf_bar_graphs(pivot_tables)

        # Create frame to hold widgets used to rerun ABF module
        self.abf_params_reveal = BorderButton(self, text='ABF Parameters', padx=4, command=self.show_params,
                                              width=16, borderwidth=0, font='-size 12', background='#00e600')
        self.abf_params_reveal.pack(side=BOTTOM, anchor='se', padx=3, pady=2)
        self.abf_variables = self.create_abf_variables()
        self.abf_params = ABFParamsPopup(self)

    def show_params(self):
        """Show the popup for rerunning ABF module"""
        self.abf_params_reveal.pack_forget()
        self.abf_params.pack(side=BOTTOM, anchor='se', padx=1)

    def hide_params(self):
        """Hide the popup for rerunning ABF module"""
        self.abf_params.pack_forget()
        self.abf_params_reveal.pack(side=BOTTOM, anchor='se', padx=3, pady=2)

    def display_abf_bar_graphs(self, pivot_tables):
        """Creates and displays charts from pivot tables

        :param pivot_tables: dict, required
            Keys are the names of the pivot tables, and values are pivot tables in the form of pandas dataframes
        :return: None
        """

        graphs = self.create_abf_bar_graphs(pivot_tables)  # Create matplotlib charts from data frames

        # Create Tk chart widgets from matplotlib charts
        for graph in graphs:
            chart_container = ChartContainer(self.abf_pivot_tables, graph, DARK_COLOR)
            chart_container.pack()

    def create_abf_bar_graphs(self, pivot_tables):
        """Creates matplotlib charts from pivot tables

        :param pivot_tables: dict, required
            Keys are the pivot table categories, and values are pivot table data in the form of pandas dataframes
        :return: list of matplotlib.figure.Figure
            Returns one figure for each pivot table
        """

        graphs = []  # List of charts to be returned at end of method

        # Chart style options
        fg_color = '#FFFFFF'
        bg_color = DARK_COLOR
        bar_width = 0.5

        # Create charts from pivot tables
        for pivot_table_category, pivot_table in pivot_tables.items():
            # Create figure
            fig = Figure(figsize=(8, 4))
            ax = fig.add_subplot(111)

            # Get information from pivot table dataframe
            categories = pivot_table.index.tolist()
            ind = list(range(len(categories)))
            ys = pivot_table[('sum', 'ptax_rev_w')].values / 10 ** 6
            title = 'State: {}. {} by {}'.format(self.winfo_toplevel().parent.general_params.state,
                                                 'Total Tax Revenue', pivot_table_category)

            # If there are fewer than 3 categories, make the chart a horizontal bar graph
            if len(categories) > 3:
                ax.bar(ind, ys, bar_width, align='center', color='#1aff8c')
                ax.set_ylabel('$ millions', fontsize=9)
                ax.set_xticks(ind)
                ax.set_xticklabels(categories)
                ax.yaxis.grid(False)
            else:
                ax.barh(ind, ys, bar_width, align='center', color='#1aff8c')
                ax.set_xlabel('$ millions', fontsize=9)
                ax.set_yticks(ind)
                ax.set_yticklabels(categories)
                ax.xaxis.grid(False)

            format_chart(fig, ax, title, bg_color, fg_color)
            graphs.append(fig)

        return graphs

    def create_abf_variables(self):
        """Create Tk variables used to rerun ABF parameter

        :return: dict,
            Returns dictionary where keys are the name of the variables and values are the variables
        """

        # Set the current value of all variables equal to the values of those same variables in the top level widget
        default = self.winfo_toplevel().parent.all_params[0]

        # Create the variables
        variables = {
            'payroll_tax': DoubleVar(value=default.payroll_tax),
            'benefits_tax': BooleanVar(value=default.benefits_tax),
            'average_state_tax': DoubleVar(value=default.average_state_tax),
            'max_taxable_earnings_per_person': IntVar(value=default.max_taxable_earnings_per_person),
            'total_taxable_earnings': IntVar(value=default.total_taxable_earnings)
        }
        return variables

    def rerun_abf(self):
        """Run the ABF again with user inputs and update ABF results"""

        # Get user input values for each parameter
        parameters = {k: v.get() for k, v in self.abf_variables.items()}

        # Rerun ABF module with new parameters
        abf_output, pivot_tables = self.abf_module.run(variables=parameters, rerun=True)

        # Update ABF summary and pivot table charts
        self.update_abf_output(abf_output, pivot_tables)

    def update_abf_output(self, abf_output, pivot_tables):
        """

        :param abf_output: dict, required
            Keys are the type of summary information, and values are floats
        :param pivot_tables: dict, required
            Keys are the pivot table categories, and values are pivot table data in the form of pandas dataframes
        :return: None
        """

        self.abf_summary.update_results(abf_output)  # Update summary frame

        # Remove previous pivot table charts
        for graph in self.abf_pivot_tables.winfo_children():
            graph.destroy()
        self.display_abf_bar_graphs(pivot_tables)  # Recreate the pivot table charts with the new data


class ABFResultsSummary(Frame):
    def __init__(self, parent, output):
        """Frame that holds ABF summary information such as total income and tax revenue

        :param parent: tk widget, required
        :param output: dict, required
            Keys are the type of summary information, and values are floats
        """

        super().__init__(parent, bg=DARK_COLOR, highlightcolor='white', highlightthickness=1, pady=8, padx=10)

        # Total income of workers
        self.income_label = Label(self, text='Total Income:', bg=DARK_COLOR, fg=LIGHT_COLOR, anchor='e',
                                  font='-size 12 -weight bold')
        self.income_value = Label(self, bg=LIGHT_COLOR, fg=DARK_COLOR, anchor='e', padx=5, font='-size 12')

        # Total revenue from taxing workers
        self.tax_revenue_label = Label(self, text='Total Tax Revenue:', bg=DARK_COLOR, fg=LIGHT_COLOR, anchor='e',
                                       font='-size 12 -weight bold')
        self.tax_revenue_value = Label(self, bg=LIGHT_COLOR, fg=DARK_COLOR, anchor='e', padx=5, font='-size 12')

        # Total revenue from taxing benefits
        self.benefits_recouped_label = Label(self, text='Tax Revenue Recouped from Benefits:', bg=DARK_COLOR,
                                             fg=LIGHT_COLOR, anchor='e', font='-size 12 -weight bold')
        self.benefits_recouped_value = Label(self, bg=LIGHT_COLOR, fg=DARK_COLOR, anchor='e', padx=5, font='-size 12')

        # Add labels to the frame
        self.income_label.grid(row=0, column=0, sticky='we', padx=3, pady=2)
        self.tax_revenue_label.grid(row=1, column=0, sticky='we', padx=3, pady=2)
        self.benefits_recouped_label.grid(row=2, column=0, sticky='we', padx=3, pady=2)
        self.income_value.grid(row=0, column=1, sticky='we', padx=3, pady=2)
        self.tax_revenue_value.grid(row=1, column=1, sticky='we', padx=3, pady=2)
        self.benefits_recouped_value.grid(row=2, column=1, sticky='we', padx=3, pady=2)

        self.update_results(output)  # Fill the labels with values from ABF output

    def update_results(self, output):
        """Fills the frame labels with values from ABF output

        :param output: dict, required
            Keys are the type of summary information, and values are floats
        :return: None
        """
        # Convert output data to string, including confidence intervals
        income = '{} (\u00B1{:,.1f}) million'.format(as_currency(output['Total Income (Weighted)'] / 10 ** 6),
                                                     0.5 * (output['Total Income Upper Confidence Interval'] -
                                                            output['Total Income Lower Confidence Interval']) / 10 ** 6)

        tax = '{} (\u00B1{:,.1f}) million'.format(as_currency(output['Total Tax Revenue (Weighted)'] / 10 ** 6),
                                                  0.5 * (output['Total Tax Revenue Upper Confidence Interval'] -
                                                         output[
                                                             'Total Tax Revenue Lower Confidence Interval']) / 10 ** 6)

        benefits_recouped = '{} million'.format(as_currency(output['Tax Revenue Recouped from Benefits'] / 10 ** 6))

        # Update the text of the income, tax, and benefits recouped labels with data strings
        self.income_value.config(text=income)
        self.tax_revenue_value.config(text=tax)
        self.benefits_recouped_value.config(text=benefits_recouped)


class ABFParamsPopup(Frame):
    def __init__(self, parent, **kwargs):
        """Popup frame that contains widgets for rerunning ABF module"""

        super().__init__(parent, bg=VERY_LIGHT_COLOR, borderwidth=1, relief='solid', padx=3, pady=3, **kwargs)

        # Create frame to hold widgets for user inputs related to benefit financing
        abf_variables = parent.abf_variables
        self.benefit_financing_frame = BenefitFinancingFrame(self, abf_variables, text='Benefit Financing:',
                                                             style='MSLabelframe.TLabelframe', wraplength=300)
        self.benefit_financing_frame.pack(fill=X, side=TOP, padx=4, pady=4)

        # Buttons to rerun ABF module and to hide this popup
        self.abf_params_buttons = Frame(self, bg=VERY_LIGHT_COLOR, pady=4)
        self.abf_params_buttons.pack(side=BOTTOM, fill=X, expand=True)
        self.abf_params_hide = BorderButton(self.abf_params_buttons, text='Hide', padx=4, command=parent.hide_params,
                                            background='#00e600')
        self.abf_params_hide.pack(side=LEFT, pady=3, padx=5)
        self.run_button = BorderButton(self.abf_params_buttons, font='-size 11 -weight bold', text="Run ABF",
                                       command=parent.rerun_abf, padx=4)
        self.run_button.pack(side=RIGHT, pady=3, padx=5)


class ProgressWindow(Toplevel):
    def __init__(self, parent, engine_process):
        """Window to show realtime progress of simulation engine"""

        super().__init__(parent)

        # Add icon to window
        self.icon = PhotoImage(file='impaq_logo.gif')
        self.tk.call('wm', 'iconphoto', self._w, self.icon)
        self.protocol("WM_DELETE_WINDOW", self.on_close)

        self.parent = parent
        self.engine_process = engine_process

        # Frame to hold all of window's contents
        self.content = Frame(self, width=100)
        self.content.pack(fill=BOTH, expand=True)

        # Progress is a value between 0 and 100. When this variable changes, the bar automatically updates.
        self.progress = DoubleVar(0)
        # Progress bar that fills up as progress value changes
        self.progress_bar = ttk.Progressbar(self.content, orient=HORIZONTAL, length=100, variable=self.progress,
                                            max=100)
        self.progress_bar.pack(fill=X, padx=10, pady=5)

        # Frame to hold text description of current simulation progress
        self.updates_container = ScrollFrame(self.content, height=30, bg=VERY_LIGHT_COLOR)
        self.updates_container.pack(fill=BOTH, expand=True)

        self.bind("<MouseWheel>", self.scroll)  # Allow scrolling with mouse wheel
        center_window(self)  # Position window in middle of the screen

    def update_progress_python(self, q):
        """Updates the progress bar and messages for Python engine

        :param q: multiprocessing.Queue, required
            Queue used to check for current status of the simulation engine
        :return: None
        """
        try:  # Try to check if there is data in the queue
            update = q.get_nowait()   # Get update from queue
            complete = self.parse_update(update, engine='Python')  # Parse the update message
            if not complete:  # If engine is still running, check for progress again after 0.5 seconds.
                self.after(500, self.update_progress_python, q)
        except queue.Empty:  # If there are no updates in the queue, do nothing and check again in 0.5 seconds
            self.after(500, self.update_progress_python, q)

    def update_progress_r(self, progress_file):
        """Updates the progress bar and messages for R engine

        :param progress_file: Text file, required
            The file to be read to get engine progress. Progress messages are separated by newlines.
        :return: None
        """

        line = progress_file.readline()   # Get update from text file
        if line == '':  # If there are no updates in the queue, do nothing and check again in 0.5 seconds
            self.after(500, self.update_progress_r, progress_file)
        else:
            update = ast.literal_eval(line)  # Convert string update to a dictionary
            complete = self.parse_update(update, engine='R')  # Parse the update message
            if not complete:
                # If engine is still running, check for progress again after 0.5 seconds.
                self.after(500, self.update_progress_r, progress_file)
            else:
                progress_file.close()

    def parse_update(self, update, engine='Python'):
        """Perform certain action depending on update from simulation engine

        :param update: dict, required
            Dictionary should contain information about the update's type and value. Valid types are "progress",
            "message", "done", "error", and "warning".
        :param engine: str, optional
            The engine type that is running. Valid engines are "Python" and "R".
        :return: bool
            Whether the engine has completed simulation or encountered an error
        """

        update_type = update['type']
        if update_type == 'progress':
            # Update progress bar
            self.progress.set(int(update['value']))
        elif update_type == 'message':
            # Add new message to updates_container
            self.add_update(update['value'], update['engine'])
        elif update_type == 'done':
            # Show results window
            self.parent.show_results(engine=engine)
        elif update_type == 'error':
            # Display error message
            error_message = '{}: {}'.format(type(update['value']).__name__, str(update['value']))
            self.add_update(error_message, update['engine'], fg='#e60000')
            self.parent.run_button.enable()  # Enable run button to allow user to run simulation again
        elif update_type == 'warning':
            # Display warning if a user's installed libraries are a lower version than recommended
            dependency, dependency_version = update['value']
            warning_message = 'Warning: {} library might need to be updated to at least version {}' \
                .format(dependency, dependency_version)
            self.add_update(warning_message, update['engine'], fg='#ff9900')

        self.update_idletasks()  # Update window with changes
        return update_type == 'done' or update_type == 'error'

    def add_update(self, message, sim_num, fg='#006600'):
        """Adds a label to updates container with the contents of an update message

        :param message: str, required
            Text that will be displayed in the updates container
        :param sim_num: int, required
            Used to inform user about which comparison program this message is associated to
        :param fg: str, optional
            Foreground color of the text label
        :return: None
        """

        # If the update is attached to a simulation number, display that number
        if sim_num is not None:
            sim_name = get_sim_name(sim_num)
            update_text = '{}: {}'.format(sim_name, message)
        else:
            update_text = message

        # Create the update label and add it to parent
        label = Message(self.updates_container.content, text=update_text, bg=VERY_LIGHT_COLOR,
                        fg=fg, anchor='w', width=350)
        label.pack(padx=3, fill=X)

        self.update_idletasks()  # Update the window to display new widget
        self.updates_container.update_scroll_region()  # Update scroll region to account for new widget space
        self.updates_container.canvas.yview_moveto(1)  # Move scroll area to bottom

    def scroll(self, event):
        """Scrolls window based on the mouse wheel event"""
        # In Windows, the delta will be either 120 or -120. In Mac, it will be 1 or -1.
        # The delta value will determine whether the user is scrolling up or down.
        move_unit = 0
        if event.num == 5 or event.delta > 0:
            move_unit = -1
        elif event.num == 4 or event.delta < 0:
            move_unit = 1

        # Scroll the updates container
        self.updates_container.canvas.yview_scroll(move_unit, 'units')

    def on_close(self):
        """Destroy this window when it is closed"""
        if self.engine_process.is_alive():
            print('Terminating simulation')
            self.engine_process.terminate()
            self.parent.run_button.enable()

        self.destroy()


# From StackOverflow: https://stackoverflow.com/questions/3221956/how-do-i-display-tooltips-in-tkinter
class ToolTipCreator:
    def __init__(self, widget, text, waittime=500, wraplength=250):
        """Tooltip that appears when user hovers the mouse over a widget

        :param widget: Tk widget, required
            The widget that will trigger the tooltip
        :param text: str, required
            The text that will appear in the tooltip
        :param waittime: int, default 500
            The number of milliseconds to wait before displaying tooltip
        :param wraplength: int, default 250
            The max width of the tool tip in pixels before text wraps to new line
        """

        self.widget = widget
        self.text = text
        self.waittime = waittime
        self.wraplength = wraplength
        self.widget.bind("<Enter>", self.enter)  # Bind action when mouse enters widget
        self.widget.bind("<Leave>", self.leave)  # Bind action when mouse leaves widget
        # If user clicks widget, it performs the same action as mouse leave
        self.widget.bind("<ButtonPress>", self.leave)
        self.id = None
        self.tw = None

    def enter(self, _=None):
        """Schedule tooltip reveal on mouse enter"""
        self.schedule()

    def leave(self, _=None):
        """Hide tooltip on mouse leave"""
        self.unschedule()
        self.hidetip()

    def schedule(self):
        """Show tooltip after waiting"""
        self.unschedule()
        self.id = self.widget.after(self.waittime, self.showtip)

    def unschedule(self):
        """Cancel tooltip reveal"""
        tooltip_id = self.id
        self.id = None
        if tooltip_id:
            self.widget.after_cancel(tooltip_id)

    def showtip(self, _=None):
        """Create a tooltip and reveal it"""
        # Get location of widget, which is used to insert the tooltip
        x, y, cx, cy = self.widget.bbox("insert")
        x += self.widget.winfo_rootx() + 25
        y += self.widget.winfo_rooty() + 20
        # Creates a top level window
        self.tw = Toplevel(self.widget)
        # Leaves only the label and removes the app window
        self.tw.wm_overrideredirect(True)
        self.tw.wm_geometry("+%d+%d" % (x, y))
        label = Label(self.tw, text=self.text, justify='left', background="#ffffff", relief='solid',
                      borderwidth=1, wraplength=self.wraplength, padx=4, pady=4)
        label.pack(ipadx=1)

    def hidetip(self):
        """Destroy tooltip window"""
        tw = self.tw
        self.tw = None
        if tw:
            tw.destroy()


class AdvancedFrame(Frame):
    def __init__(self, parent, toggle_function, **kwargs):
        """

        :param parent: Tk widget, required
        :param toggle_function: function, required
            Function to be performed when on and off buttons are clicked
        :param kwargs: Other widget options
        """

        super().__init__(parent, **kwargs)

        # Label to describe the functions of the buttons
        tip = 'Reveal advanced simulation parameters'
        self.advanced_label = TipLabel(self, tip, text="Advanced Parameters:", bg=DARK_COLOR, fg=LIGHT_COLOR,
                                       font='-size 9 -weight bold')

        if sys.platform == 'darwin':
            self.button_container = Frame(self, background=DARK_COLOR)
        else:
            self.button_container = Frame(self, highlightbackground='#FFFFFF', borderwidth=1, relief='flat')

        # Buttons that reveal or hide the advanced parameters
        self.on_button = SubtleToggle(self.button_container, text='On', width=3, command=toggle_function)
        self.off_button = SubtleToggle(self.button_container, text='Off', width=3, command=toggle_function)
        self.off_button.toggle()

        # Add all widgets to the frame
        self.advanced_label.pack(side=LEFT, anchor=E, fill=BOTH)
        self.button_container.pack(side=LEFT, anchor=E)
        self.on_button.pack(side=LEFT)
        self.off_button.pack(side=LEFT)


class SubtleButton(Button):
    def __init__(self, parent=None, foreground='#FFFFFF', font='-size 8 -weight bold', pady=2, padx=3,
                 relief='flat', highlightthickness=0, borderwidth=0, background=DARK_COLOR, **kwargs):
        """Button that blends in with the background"""
        super().__init__(parent, background=background, activebackground=background, foreground=foreground,
                         activeforeground=foreground, font=font, pady=pady, padx=padx, relief=relief,
                         highlightthickness=highlightthickness, borderwidth=borderwidth, **kwargs)


class SubtleToggle(SubtleButton):
    """Button that blends in with the background when it is 'off' and has a a different background when it is 'on'"""
    def __init__(self, parent=None, on_background='#00CC00', off_background=DARK_COLOR, **kwargs):
        super().__init__(parent, **kwargs)

        self.toggled_on = False  # Button is off by default
        self.on_background = on_background
        self.off_background = off_background

    def toggle(self):
        """Changes the background of the button based on the current state"""
        if self.toggled_on:  # If button is on, then turn off
            self.toggled_on = False
            if sys.platform == 'darwin':
                self.config(relief=SUNKEN)
            else:
                self.config(background=self.off_background)
        else:  # If button is off, then turn on
            self.toggled_on = True
            if sys.platform == 'darwin':
                self.config(relief=RAISED)
            else:
                self.config(background=self.on_background)


class BorderButton(Frame):
    def __init__(self, parent=None, custom=False, background='#0074BF', font='-size 11', width=7, pady=0,
                 foreground='#FFFFFF', activebackground='#FFFFFF', relief='flat', highlightthickness=1, borderwidth=1,
                 highlightbackground=LIGHT_COLOR, **kwargs):
        """A frame that imitates a button that has a border around it

        :param parent: Tk widget
        :param custom: bool, default False
            If set to false, the frame will not be prepopulated with a default button. A custom button can be added.
        :param options: Other widget options
        """

        if sys.platform == 'darwin':
            highlightbackground = DARK_COLOR
            borderwidth = 0
            foreground = None
            background = None

        super().__init__(parent, highlightbackground=highlightbackground, relief=relief, borderwidth=borderwidth)

        if not custom:
            # If not custom, then add a default button to frame
            self.button = Button(self, foreground=foreground, background=background, font=font, width=width,
                                 relief='flat', activebackground=activebackground, pady=pady, borderwidth=0,
                                 highlightthickness=highlightthickness, **kwargs)
            self.button.pack()
        else:
            # If custom, do not add a button now. A button will need to be added later.
            self.button = None

    def add_content(self, content):
        """Adds a custom button to the frame instead of a default one

        :param content: Tk Button, required
            The button that will populate the frame
        :return: None
        """
        self.button = content
        content.pack()


class RunButton(BorderButton):
    def __init__(self, parent=None, **kwargs):
        """A button that can be disabled or enabled with visual indicators for each state

        :param parent: Tk widget
        :param kwargs: Other widget options
        """
        super().__init__(parent, custom=True)
        button = Button(self, foreground='#FFFFFF', background='#ccebff', font='-size 11 -weight bold', width=8,
                        relief='flat', activebackground='#FFFFFF', disabledforeground='#FFFFFF', state=DISABLED,
                        highlightthickness=0, borderwidth=0, pady=1, **kwargs)
        self.add_content(button)

    def enable(self):
        """Enable button and change background to indicate state"""
        self.button.config(state=NORMAL, bg=THEME_COLOR)

    def disable(self):
        """Disable button and change background to indicate state"""
        self.button.config(state=DISABLED, bg='#99d6ff')


class GeneralEntry(Entry):
    def __init__(self, parent=None, **kwargs):
        """Entry with default styling"""
        super().__init__(parent, borderwidth=2, highlightbackground='#FFFFFF', relief='flat',
                         highlightthickness=1, font='-size 11', **kwargs)


class NotebookEntry(Entry):
    def __init__(self, parent=None, font='-size 11', **kwargs):
        """Entry with default styling"""
        super().__init__(parent, borderwidth=2, highlightbackground='#999999', relief='flat',
                         highlightthickness=1, font=font, **kwargs)


class TipLabel(Label):
    def __init__(self, parent=None, tip='', cursor='question_arrow', **kwargs):
        """Label that will reveal a tooltip when cursor is hovered over it

        :param parent: Tk widget
        :param tip: str, default ''
            The text that will appear in the tooltip
        :param cursor: str, default 'question_arrow'
            The type of cursor that appears when hovering over the label
        :param kwargs: Other widget options
        """

        super().__init__(parent, cursor=cursor, **kwargs)
        ToolTipCreator(self, tip)


class TipCheckButton(ttk.Checkbutton):
    def __init__(self, parent=None, tip='', cursor='question_arrow', onvalue=True, offvalue=False,
                 style='MSCheckbutton.TCheckbutton', **kwargs):
        """Check button that will reveal a tooltip when cursor is hovered over it

        :param parent: Tk widget
        :param tip: str, default ''
            The text that will appear in the tooltip
        :param cursor: str, default 'question_arrow'
            The type of cursor that appears when hovering over the label
        :param onvalue: bool, default True
        :param offvalue: bool, default False
        :param style: str, default 'MSCheckbutton.TCheckbutton'
        :param kwargs: Other widget options
        """

        super().__init__(parent, cursor=cursor, onvalue=onvalue, offvalue=offvalue, style=style, **kwargs)
        ToolTipCreator(self, tip)


def run_engine_python(se, q):
    """Run the Python engine

    :param se: SimulationEngine, required
        Instance of Python simulation engine to be run
    :param q: multiprocessing.Queue
        Queue used to check for current status of the simulation engine
    :return: None
    """

    se.run()
    # When engine is done, add a message to the queue to notify progress window
    q.put({'type': 'done', 'value': 'done'})


def run_engine_r(command):
    """Run the R engine

    :param command: str, required
        The command that will be run in the subprocess
    :return: None
    """

    # Run the engine as a command in a new process
    subprocess.run(command, shell=True, cwd='./r_model_full')
