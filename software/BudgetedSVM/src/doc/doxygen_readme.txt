In order to run Doxygen to generate documentation, type the following command in the command prompt:

>> ./doxygen_1.8.2.exe budgetedSVM.dox

Doxygen will then generate two folders: 'html' and 'latex'.


Generating HTML documentation
=============================
No need to do anything else. Simply open file 'html/index.html' in your browser to access documentation.


Generating PDF documentation
============================
In order to generate PDF version of documentation, you need to run file 'make.exe' located in 'latex' 
folder. However, before doing so, in file 'mainpage.dox' replace line
"\mainpage <a href="http://www.dabi.temple.edu/budgetedsvm/">BudgetedSVM</a> Documentation"
by 
"\mainpage BudgetedSVM Documentation"
without quotes, so that PDF is generated without HTML code used when generating HTML documentation. Note
that you might need to delete 'latex' folder and re-run './doxygen_1.8.2.exe budgetedSVM.dox' before
running 'latex/make' in order to account for the change.

Known issues:
* When the authors run 'make' we got an error related to missing 'listings.sty'. The solution can be
found at 'http://tex.stackexchange.com/questions/126514/miktex-fails-to-download-the-listings-package'.