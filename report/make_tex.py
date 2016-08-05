import subprocess
import os
import sys

def get_filepaths(directory):
	"""
	This function will generate the file names in a directory 
	tree by walking the tree either top-down or bottom-up. For each 
	directory in the tree rooted at directory top (including top itself), 
	it yields a 3-tuple (dirpath, dirnames, filenames).
	"""
	file_paths = []  # List which will store all of the full filepaths.

	# Walk the tree.
	for root, directories, files in os.walk(directory):
		for filename in files:
			# Join the two strings in order to form the full filepath.
			filepath = os.path.join(root, filename)
			file_paths.append(filepath)  # Add it to the list.

	return file_paths  # Self-explanatory.

def getTexStringFor(full_file_paths, title, sectiontype, parentsection):
	title_str = "\\" + sectiontype + "{"
	end_title = "}\n"
	end_str = "\n\\end{minted}"
	page_break = "\\pagebreak\n"
	total_str = "\\" + parentsection + "{" + title + "}\n"
	for f in full_file_paths:
		basename = os.path.basename(f)
		if not f.endswith(".ll") and (not f.endswith('.DS_Store')) and (not f.endswith('.log')) and basename != 'dice' and ("Hello_World_Demo" not in f) and not f.endswith('.pdf'):
			total_str += title_str + basename.replace("_", "\\_") + end_title

			minted_type = "text"
			if f.endswith(".dice"):
				minted_type = "java"
			elif f.endswith(".sh"):
				minted_type = "bash"
			elif f.endswith(".ml") or f.endswith(".mll") or f.endswith(".mly"):
				minted_type = "ocaml"

			total_str += "\\inputminted[breaklines,linenos]{" + minted_type + "}{" + f.replace('\\', '/') + "}\n"
			total_str += page_break	
			# print(basename)

	return total_str

def writeTexFile(title, directory, texname, sectiontype, parentsection):
	full_file_paths = get_filepaths(directory)
	content = getTexStringFor(full_file_paths, title, sectiontype, parentsection)
	f = open('Code/' + texname, 'w')
	print("Code/" + texname)
	f.write(content)
	f.close()

def writeGitLog():
	pr = subprocess.Popen( "git log" , cwd = os.path.dirname(os.path.realpath(__file__)), shell = True, stdout = subprocess.PIPE, stderr = subprocess.PIPE )
	(out, error) = pr.communicate()
	f = open('Includes/gitlog.tex', 'w')
	total_str = "\\begin{minted}[linenos]{text}\n"
	total_str += out.decode("utf-8")
	total_str += "\n\\end{minted}\n"
	f.write(total_str)
	f.close()

# writeTexFile("Test Suite Code", "../tests/", 'tests.tex', 'subsection', 'section')
writeTexFile("Code", "../src/", "code.tex", 'subsection', 'section')
writeTexFile("Demo", "../Demo/", "demo.tex", 'subsection', 'section')
writeGitLog()