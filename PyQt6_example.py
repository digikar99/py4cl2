# This is an example of running a GUI application in Python
# while still having interaction to the running Lisp REPL.
# We poll for messages from the REPL, like how Jupyter
# notebooks work.
# Run me as:
# (raw-py-exec/no-return "import PyQt6_example; PyQt6_example.start_app(try_process_message);")
def start_app (try_process_message):
        import PyQt6
        import sys
        import matplotlib
        import matplotlib.pyplot as plt
        from PyQt6.QtWidgets import QApplication
        from PyQt6.QtCore import QTimer
        app = QApplication(sys.argv)

        matplotlib.use("QtAgg")
        plot = plt.plot([1, 2, 3],[4, 5, 6])
        plt.show(block=False)

        timer = QTimer()
        def process_messages():
                try_process_message(blocking=False)
        timer.timeout.connect(process_messages);
        timer.start(100);
        print("Going into main loop, will return when all windows closed")
        app.exec()
        print("No more windows, returning to default messsage_dispatch_loop")
