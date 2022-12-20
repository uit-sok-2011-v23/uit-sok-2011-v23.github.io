
{% include navbar_open.html %}{% include top-box.html %}
<br><br><br>
<div align="right">
<a href="readme_no.html"><img src="https://uit-econ.github.io/images/GB.png"></a>
</div>

# How to set up a course page

## 1. Create an organization

The first thing we need to do is to create an "organization", which will own the course page. You do this as follows:

1. Log in to github.com
2. Go to https://github.com/organizations/plan
3. Select "Create organization" 
4. Choose the "Create a free organization" option <br><br> ![image](https://uit-econ.github.io/images/createfreeorg.png)<br><br>
5. Use the following setup:
	1. Name your organization using the naming convention<br><br> 
	`uit-<course_code>-<semester><year>`.<br><br>`<course_code>` can for example be SOK-1006.<br>`<semester>` can be either "h" or "v" (Norwegian) or "f" or "s" (English).<br>Use the last two digits in `<year>`.<br><br>
	For example, the name can be `uit-sok-1006-v23`.<br><br>
	3. Enter your e-mail
	4. Select "A business or institution"
	5. Enter "UiT The Arctic University of Norway" as name of institution.<br><br>
	6. Accept the terms and click "Next"<br><br> ![image](https://uit-econ.github.io/images/setup.png)<br><br>
6. Add the github usernames of the colleagues that are involved in the course and click "Complete setup"<br><br> ![image](https://uit-econ.github.io/images/addcolleagues.png)
7. If needed, enter your github password <br><br> ![image](https://uit-econ.github.io/images/password.png)
8. Click your icon in the upper right corner, and select "Your organizations"<br><br> ![image](https://uit-econ.github.io/images/selectorganizations.png)<br><br>
9. Click on the organization you just created<br><br>
		
## 2. Create the course repository (repo)

1. Go to this repository: https://github.com/uit-econ/coursepage_template 
2. Click "Use this template" and "Create new repository" ![image](https://uit-econ.github.io/images/createnewrepo.png)
3. **IMPORTANT!** Change the "Owner" to the organization you just created. <br>![image](https://uit-econ.github.io/images/reposettings.png)
4. Name the repo exactly the same as the organization name, but append ".github.io" to the name.<br>
For example, if your organization name was "uit-sok-1006-v23", the course repository should be "uit-sok-1006-v23.github.io".<br>
The purpose of this is to make the repository the default page of the organization. 
5. Select the "Public" option and click "Create repository" 
		
## 3. View the repo homepage
1. Locate the "github-pages" link down to the right in your repository, and click it.<br><br>
![image](https://uit-econ.github.io/images/githubpages.png)<br><br>
3. Click on the "View deployment" in the following page, and you will see your new course page (can take a couple of minutes before it is ready, so you might have to refresh the brower a couple of times)
4. If you cannot see the "github-pages" link, even after waiting for a few minutes, enable the web interface by clicking "Settings" in the repo menu, "Pages" in the left pane and "main" in the "Branch" dropdown. Then click "Save".<br><br>
			
## 4. Edit the repo
1. Edit the settings-file "\_config.yml" by clicking on the pen-icon up to the right. <br><br> You only need to edit the first three lines (unless you want to edit the advanced settings). The first three lines are:

	* **name: Sok-xxxx Emnetittel**: <br>
	Required. Must be changed to the current course code and course name.<br><br>
	* **semester: Høst/Vår 20xx**:<br>
	Required. Must be changed to the current semester.<br><br>
	* **image: tema.jpg**:<br>
	Optional. You can upload another picture if you want. If it has another name than "tema.jpg", you need to change this to the correct name. However you can also just delete "tema.jpg" and upload a new image with the same name. ![image](https://uit-econ.github.io/images/editconfig0.png)<br><br>


2. Edit the left menu of the homepage by editing the file "navbar.html". Begin by removing the link to this document ("Hvordan sette opp en kursside").
You can edit existing links by changing the link address and the link text. If you need more items, just copy one of the links and change it.<br><br> 
**NOTE!** The markup files are automatically converted to html-files. If you want to link to a markdown \*.md-file, you must change the file extension from ".md" to ".html". <br><br> ![image](https://uit-econ.github.io/images/editnavigate.png)

3. There are all ready templates in Norwegian for the start page ("start.md"), lecture plan ("forelesningsplan.md"),
tutorial plan ("seminarplan.md") and plan for submissions ("innleveringer.md"). You can see from these examples how you create links and tables.<br><br>
**IMPORTANT!** If you create new markup (.md) files, always put **\{\% include navbar_open.html \%\}\{\% include top-box.html \%\}** in the top of the document. This ensures that the left menu and the top heading box are loaded with the page.<br><br>
The exception from this rule is "start.md" that should ***not*** have the curly bracket expression in the top.<br><br>
The files "index.md" and "index_open.md" should not be edited. <br><br>

## 5. Edit the *Samfunnsøkonomi med datavitenskap* main page
In order for your course page to be available from the [main page](https://uit-econ.github.io/), you need to edit the corresponding item there. 

1. Go to the `\_portfolio` folder in the `uit-econ.github.io` repository in the main page organization `uit-econ` here: [uit-econ/uit-econ.github.io/\_portfolio/](https://github.com/uit-econ/uit-econ.github.io/tree/main/_portfolio)<br><br>![image](https://uit-econ.github.io/images/editmainpage.png)<br><br>
2. Click on the course you are responsible for, and edit it (click on pen symbol up to the right).<br><br>![image](https://uit-econ.github.io/images/editmainpage2.png)<br><br>
3. The document you just opened may contain a course description, or not. <br>
	* If it does not contain a course description, you will find a code that look like this in the document:<br><br>
	<div style="background-color:#f6f8fa;font-family:Courier; padding-left:160"><br>
	---<br>
	&#123;% include nettsideApnerTop.html %&#125;<br>
	window.open('replace with the link to your github course page');<br>
	<br></div><br><br>
	Replace the link with the one to the homepage you created in step 3.<br><br>
	* If it does contain a course description, you will not find the link above. The page in edit mode will look like this:<br><br>![image](https://uit-econ.github.io/images/editmainpage_content.png)<br><br>
	In that case, remove the description (copy it and use it on course homepage you created if you want) and paste the code above, with the course page address in stead of `replace with the link to your github course page`.  <br><br>
	4. When you save (commit) you will get a question about making a "pull request", if you do not have write privileges. Push the button and ask a collegue with write privileges to accept your "pull request". 

## 6. Embed course page into the Canvas room
This will show you how you embed the course page into Canvas. 
1. Go to [Canvas](https://uit.instructure.com/) and select the course you are responsible for <br><br>![image](https://uit-econ.github.io/images/canvasorig.png)<br><br>
2. Click the "Rediger"/"Edit" button up to the right, and then click the  "< >" (edit html) symbol down to the right <br><br>![image](https://uit-econ.github.io/images/canvashtmledit.png)<br><br>
3. Delete all the html-contents, and paste in<br><br>
	<div style="background-color:#f6f8fa;font-family:Courier; padding-left:80"><br>
		&lt;p&gt;&lt;iframe style="overflow: hidden;"<br>
		src="replace with the link to your github homepage"<br>
		width="1500" height="3000"&gt;&lt;/iframe&gt;&lt;/p&gt;<br>
		<br>
	</div><br><br>![image](https://uit-econ.github.io/images/canvashtmlnew.png)<br><br>
4. Replace `replace with the link to your github homepage` with the link to the homepage you created in step 3.<br><br>
			

**YOUR DONE!!!!**
		
