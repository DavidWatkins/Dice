import os

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

# Run the above function and store its results in a variable.   
full_file_paths = get_filepaths("Test Suite/")

begin_str = "\\begin{minted}[breaklines,linenos]{java}\n"
title_str = "\\subsection{"
end_title = "}\n"
end_str = "\n\\end{minted}"
page_break = "\\pagebreak\n"

total_str = "\\section{Test Suite Code}\n"

for f in full_file_paths:
	basename = os.path.basename(f)
	if not f.endswith(".ll") and (not f.endswith('.DS_Store')) and (not f.endswith('.log')) and basename != 'dice':
		if f.endswith(".out"):
			begin_str = "\\begin{minted}[breaklines,linenos]{text}\n"
		elif f.endswith(".dice"):
			begin_str = "\\begin{minted}[breaklines,linenos]{java}\n"
		elif f.endswith(".sh"):
			begin_str = "\\begin{minted}[breaklines,linenos]{bash}\n"

		new_file = 'Final Report/Tests/' + basename + ".tex"
		tex_name = 'Test/' + basename + '.tex'
		with open(f, 'r') as content_file:
			with open(new_file, 'w') as new_file_f:
				basename = basename.replace('_', '\\_')
				content = content_file.read()
				title = title_str + basename + end_title
				content = title + begin_str + content + end_str
				new_file_f.write(content)
				new_file_f.close()
				total_str += "\input{" + tex_name + "}\n" + page_break


f = open('Final Report/Code/tests.tex', 'w')
f.write(total_str)
f.close()