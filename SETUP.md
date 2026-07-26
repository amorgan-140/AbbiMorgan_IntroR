# Setup Instructions for Students

Follow these steps to set up and publish your research website.

## Prerequisites

Before you begin, make sure you have the following:

* A GitHub account (free at [github.com](https://github.com))
* GitHub Desktop installed on your computer
* R and RStudio

## Step-by-Step Setup

### 1. Create Your Repository

1. Click the **"Use this template"** button.
2. Select **Create a new repository**.
3. Use the **Owner** dropdown menu to select your github user
4. Name the repository. **This will be in your web address!** Choose something professional like your name.
5. Click **Create repository from template**.
6. You should now have this newly created repository on your own GitHub.

### 2. Clone the Repository

1. On your computer: Make sure the **GitHub Desktop** is open.
2. On GitHub: In your own repository on GitHub, Click the **"<> Code"** button.
3. Select **Open with GitHub Desktop**.
4. Choose a local path for the placement of the directory (not a cloud service). For example: (/Users/YourUserame/GitHub/introR2026)
5. Select **Clone**. 

### 3. Edit Your Files

>[!IMPORTANT]
> 1. Open RStudio
> 2. Create a new Rproj file in the existing local directory of your repository
> 3. Open .Rproj file
> 4. Edit the following files in RStudio

#### Edit `index.qmd`

* Replace **"Your Name"** with your name.
* Update the **About Me** section.
* Add your research interests.
* Replace **"your photo.jpg"** with the filename of a photo of yourself & by adding the photo to the directory.
* Update places that say **"link"** with the correct internet link or email address, if desired.

#### Edit `projects.qmd`

* Update the descriptions of your projects.
* Add links to additional projects as you create them.

#### Edit `projects/final-report.qmd`

* Change the title to your project title.
* Write your final report.
* Replace all placeholder text with your own content.

#### (Optional) Customizations

`styles.css`
* Customize colors, fonts, and other styling options if desired.

`_quarto.yml`
* add new links to the *navbar*

`README.md`
* This is a place to add information to the GitHub reponsitory, but will not be displayed on your rendered website.

`.gitignore`
* List files here you don't want synced to GitHub. For example, if you want to keep your data.csv files local and not online list them here

`SETUP.md`
* These are instructions to create this site. No need to modify this file. 

### 4. Preview Your Website Locally

In RStudio, you can preview your website before publishing.

Run the following command in the RStudio Terminal:

```bash
quarto preview
```

To render the website before pushing your changes, run:

```bash
quarto render
```

### 5. Push Your Changes to GitHub

When you are satisfied with your changes:

1. Open **GitHub Desktop**.
2. Review the files you modified.
3. Enter a commit summary and an optional description.
4. Click **Commit to main**.
5. Click **Push origin** to upload your changes to GitHub.

### 6. Enable GitHub Pages

1. Go to your repository on GitHub.
2. Click **Settings**.
3. Select **Pages** from the left sidebar.
4. Under **Build and deployment**:

   * **Source:** Select **Deploy from a branch**.
   * **Branch:** Select **main**.
   * **Folder:** Select **/docs**.
5. Click **Save**.

### 7. View Your Website

1. Wait 1–2 minutes for GitHub to deploy your site.

2. Return to **Settings → Pages**.

3. You should see a URL similar to:

   ```
   https://YOUR-USERNAME.github.io/YOUR-REPOSITORY/
   ```

4. Click the link to view your website.

## Updating Your Website

Whenever you want to update your website:

1. Edit your `.qmd` files.
2. Commit and push your changes using GitHub Desktop.
3. Wait 1–2 minutes for GitHub Pages to rebuild your site.
4. Refresh your browser to view the updated website.

## Checking the Build Status

If your website is not updating:

1. Go to your GitHub repository.
2. Click the **Actions** tab.
3. Open the most recent workflow run.
4. Review any error messages if the build failed.

## Troubleshooting

### GitHub Pages is not enabled

* Go to **Settings → Pages**.
* Confirm that:

  * The branch is set to **main**.
  * The folder is set to **/docs**.
* Your website URL should appear on this page after deployment.

### Links or file paths are not working

Always use forward slashes (`/`) in links, even when using Windows.

```text
# Correct
[Link](projects/final-report.qmd)

# Incorrect
[Link](projects\final-report.qmd)
```

## Adding More Pages

To add another page, such as a poster, publication, or additional project:

1. Create a new `.qmd` file in the `projects/` folder.
2. Edit `projects.qmd` and add a link to the new page.
3. Commit and push your changes.
4. Wait 1–2 minutes for GitHub Pages to update your website.

## Resources

* [Quarto Documentation](https://quarto.org/)
* [GitHub Pages Documentation](https://docs.github.com/en/pages)
* [Markdown Guide](https://www.markdownguide.org/)
* [Git Basics](https://git-scm.com/book/en/v2/Getting-Started-Git-Basics)

---

**Need help?** Contact your instructor or refer to the resources listed above.
