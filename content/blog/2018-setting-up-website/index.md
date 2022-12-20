---
author: Andrea Rau
categories:
- Tips and tricks
date: "2018-02-22"
draft: false
excerpt: Moving my professional homepage 
  from a free Wordpress page to GitHub Pages using blogdown (and more
  recently, hugo apero).
layout: single
subtitle: Moving from WordPress to blogdown (to apero)
title: Setting up my website
---


I recently decided that I wanted to move my professional homepage from a free page set up on [WordPress](http://andrearau.wordpress.com) to [GitHub Pages](https://pages.github.com/) using [blogdown](https://github.com/rstudio/blogdown) by [Yihui Xi](https://yihui.name/en). 

There were basically two reasons for this: (1) Because I only sprang for the free WordPress site, there are gigantic, ugly ads that appear on every single page. I only recently realized this as I was usually viewing my WordPress site while being logged on -- and apparently, the ads only appear for other people. Needless to say, I found this very unappealing, but am also not willing to pay a huge subscription fee. (2) I love coding in R, and do a lot of my analysis work in reproducible [Rmarkdown](https://rmarkdown.rstudio.com/) documents, and as such it seemed like it would be an easy move over to `blogdown` -- this means I would be able to maintain my website in R, have a lot more flexibility and control over the look of my website, and avoid those ugly WordPress ads. 

To set up my new GitHub Pages website, I found the following two blogs to be extremely helpful:

- https://tclavelle.github.io/blog/blogdown_github/
- https://alison.rbind.io/post/up-and-running-with-blogdown/

I basically went through the first link step-by-step to create my website. To summarize:

1. I created a new repository for my blogdown source content.
2. I installed Hugo using the `install_hugo()` function from `blogdown`.
3. I built the template for the new site using `blogdown::new_site(theme = "gcushen/hugo-academic", theme_example = TRUE).
4. I changed the `config.toml` configuration file to set the `baseurl` to my GitHub URL using `baseurl = "https://<username>.github.io/"`, and I told blogdown which directory to place the rendered site in using `publishDir = "../<username>.github.io".`
5. Finally, I can create new content using the `new_post()` and `new_content()` functions from `blogdown`.
6. I can then render the site locally using `serve_site()`, and I can build the site using `build_site()`. After building the site and staging all changed files to both repsoitories, all that's left to do is commit changes and push the new site to GitHub.

For hosting it on GitHub, I used the second option from [this link](https://tclavelle.github.io/blog/blogdown_github/) -- namely, I have two repositories (including an extra one to store the source content and tell `blogdown` to publish the site to my `username.github.io` repository each time I build it, meaning I have to remember to push both repositories to GitHub when I make updates). The only other change from the tutorial I used was to add the hidden file `.nojekyll` to my `username.github.io` repository to make sure that GitHub does not rebuild the website using Jekyll.

---

As an addendum to this post, I also followed Ming Tang's [instructions](https://divingintogeneticsandgenomics.rbind.io/post/hugo-academic-theme-blog-down-deployment-some-details/) for customizing my website, primarily by changing the `config.toml` file. Notably, I changed the menu bar from white to black, added a CV section, and inactivated the selected publication and contact widgets.

---

As a second addendum to this post, I also decided to set up a custom domain for this website. I had previously purchased the domain (www.andrea-rau.com) through WordPress, without ever even really understanding how it worked. As it was coming up for renewal, I decided to transfer it to [Google Domains](https://domains.google/), which was a very straightforward process (I essentially clicked the link for "I'd like to transfer instead", followed the instructions, clicked the appropriate link sent to my email, and voila!). Once I had my domain in my Google Domains account (it took a couple of days for the transfer to complete), the process for setting up the custom domain for my website was very easy -- although I actually found the GitHub documentation to be somewhat hard to follow. I instead followed most of the steps listed [here](https://medium.com/employbl/launch-a-website-with-a-custom-url-using-github-pages-and-google-domains-3dd8d90cc33b), with the only exception being that I made sure to use the updated GitHub IP addresses:

1. Add a `CNAME` file in the `MYUSERNAME.github.io` repository that looked like the following, and committed and pushed the changes to my repo:
```
andrea-rau.com
www.andrea-rau.com
```
2. In the Manage window of Google Domains for my domain, I clicked DNS and scrolled to the bottom to the `Custom resource records` box. There I added one `@` record of type `A` for the following four IP addresses, as described in the [GitHub Pages help](https://help.github.com/articles/setting-up-an-apex-domain/): 

- `185.199.108.153`
- `185.199.109.153`
- `185.199.110.153`
- `185.199.111.153`

Then I added one `www` record of type `CNAME` for my GitHub Pages address, `MYUSERNAME.github.io`.

3. Finally, back in the Settings of my GitHub pages repo, I made sure to check the box for `Enforce HTTPS`.

---

As a third addendum to this post, I changed the font family and size per the nice instructions provided by [Alison Hill](https://discourse.gohugo.io/t/hugo-academic-theme-how-to-change-the-font-size/17294/2).

--- 

As a fourth (!) addendum to this post, I converted my whole website to hugo apero using [these instructions](https://hugo-apero-docs.netlify.app/). It took me forever to get it configured how I want (all posts about converting a previous hugo academic website were totally worthless to me), but I'm happy with it now! I now deploy to Netlify as described [here](https://hugo-apero-docs.netlify.app/start/deploy/), and I changed my custom Google domain to point to the new [website]() rather than my previous one using [these instructions](https://dev.to/lost_semicolon/netlify-and-google-domains-hm3).

A major part of all of this is that I am now automating the lists of my publications, research projects, and students through the use of a Google sheets spreadsheet that I am parsing in custom R code. Maybe one day I will describe my process in another post!
