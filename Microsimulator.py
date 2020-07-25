from GUI import MicrosimGUI
import multiprocessing

if __name__ == '__main__':
    multiprocessing.freeze_support()
    gui = MicrosimGUI()
    gui.mainloop()

