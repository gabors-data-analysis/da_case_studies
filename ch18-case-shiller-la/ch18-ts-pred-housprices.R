




<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
  <link rel="dns-prefetch" href="https://github.githubassets.com">
  <link rel="dns-prefetch" href="https://avatars0.githubusercontent.com">
  <link rel="dns-prefetch" href="https://avatars1.githubusercontent.com">
  <link rel="dns-prefetch" href="https://avatars2.githubusercontent.com">
  <link rel="dns-prefetch" href="https://avatars3.githubusercontent.com">
  <link rel="dns-prefetch" href="https://github-cloud.s3.amazonaws.com">
  <link rel="dns-prefetch" href="https://user-images.githubusercontent.com/">



  <link crossorigin="anonymous" media="all" integrity="sha512-jFUBCdWOA1Ov3xo3oFMBwsdP4Up2K1bRnP4QYI5WqvpaIYxWVek89k2M0oyTbNhYMViGtxJB3Vdwcw8ln8hGQw==" rel="stylesheet" href="https://github.githubassets.com/assets/frameworks-8c550109d58e0353afdf1a37a05301c2.css" />
  
    <link crossorigin="anonymous" media="all" integrity="sha512-juQiH9CmCfxdyMiD0Ih3b42gydxF8Nihmv5aJ4s1VAnfidy3BrBjCA5iVpH474DZu+Jzn70lfScCjDXrBoX0NQ==" rel="stylesheet" href="https://github.githubassets.com/assets/github-8ee4221fd0a609fc5dc8c883d088776f.css" />
    
    
    
    


  <meta name="viewport" content="width=device-width">
  
  <title>da_case_studies/ch18-ts-pred-housprices.R at master · gabors-data-analysis/da_case_studies</title>
    <meta name="description" content="Codes for case studies for the Bekes-Kezdi Data Analysis textbook - gabors-data-analysis/da_case_studies">
    <link rel="search" type="application/opensearchdescription+xml" href="/opensearch.xml" title="GitHub">
  <link rel="fluid-icon" href="https://github.com/fluidicon.png" title="GitHub">
  <meta property="fb:app_id" content="1401488693436528">
  <meta name="apple-itunes-app" content="app-id=1477376905">

    <meta name="twitter:image:src" content="https://avatars2.githubusercontent.com/u/42837673?s=400&amp;v=4" /><meta name="twitter:site" content="@github" /><meta name="twitter:card" content="summary" /><meta name="twitter:title" content="gabors-data-analysis/da_case_studies" /><meta name="twitter:description" content="Codes for case studies for the Bekes-Kezdi Data Analysis textbook - gabors-data-analysis/da_case_studies" />
    <meta property="og:image" content="https://avatars2.githubusercontent.com/u/42837673?s=400&amp;v=4" /><meta property="og:site_name" content="GitHub" /><meta property="og:type" content="object" /><meta property="og:title" content="gabors-data-analysis/da_case_studies" /><meta property="og:url" content="https://github.com/gabors-data-analysis/da_case_studies" /><meta property="og:description" content="Codes for case studies for the Bekes-Kezdi Data Analysis textbook - gabors-data-analysis/da_case_studies" />



  

  <link rel="assets" href="https://github.githubassets.com/">
    <link rel="shared-web-socket" href="wss://alive.github.com/_sockets/u/54452084/ws?session=eyJ2IjoiVjMiLCJ1Ijo1NDQ1MjA4NCwicyI6NTczOTMxMTczLCJjIjo2ODYzMjEwNjQsInQiOjE1OTg4ODA3NDB9--832dc0e31446f12eb110f70c3624aec2b270d0a2ef0c7c37a9087714a3536de1" data-refresh-url="/_ws">
  <link rel="sudo-modal" href="/sessions/sudo_modal">

  <meta name="request-id" content="C95E:3FBE:E99F20:190DDD4:5F4CFBD7" data-pjax-transient="true" /><meta name="html-safe-nonce" content="601f0d9c4b8da4d7028f96a9e3fb59ca8f2b8dea" data-pjax-transient="true" /><meta name="visitor-payload" content="eyJyZWZlcnJlciI6Imh0dHBzOi8vZ2l0aHViLmNvbS9nYWJvcnMtZGF0YS1hbmFseXNpcy9kYV9jYXNlX3N0dWRpZXMvdHJlZS9tYXN0ZXIvY2gxOC1jYXNlLXNoaWxsZXItbGEiLCJyZXF1ZXN0X2lkIjoiQzk1RTozRkJFOkU5OUYyMDoxOTBEREQ0OjVGNENGQkQ3IiwidmlzaXRvcl9pZCI6Ijc3NzY0NDkwMzE1OTA1NjUyODAiLCJyZWdpb25fZWRnZSI6ImlhZCIsInJlZ2lvbl9yZW5kZXIiOiJpYWQifQ==" data-pjax-transient="true" /><meta name="visitor-hmac" content="6e140f0cdb1e10af30e39fdaceab2885f2b8d36e1d40d8b6e4f971b5a421db6d" data-pjax-transient="true" /><meta name="cookie-consent-required" content="false" />

    <meta name="hovercard-subject-tag" content="repository:262293514" data-pjax-transient>


  <meta name="github-keyboard-shortcuts" content="repository,source-code" data-pjax-transient="true" />

  

  <meta name="selected-link" value="repo_source" data-pjax-transient>

    <meta name="google-site-verification" content="c1kuD-K2HIVF635lypcsWPoD4kilo5-jA_wBFyT4uMY">
  <meta name="google-site-verification" content="KT5gs8h0wvaagLKAVWq8bbeNwnZZK1r1XQysX3xurLU">
  <meta name="google-site-verification" content="ZzhVyEFwb7w3e0-uOTltm8Jsck2F5StVihD0exw2fsA">
  <meta name="google-site-verification" content="GXs5KoUUkNCoaAZn7wPN-t01Pywp9M3sEjnt_3_ZWPc">

  <meta name="octolytics-host" content="collector.githubapp.com" /><meta name="octolytics-app-id" content="github" /><meta name="octolytics-event-url" content="https://collector.githubapp.com/github-external/browser_event" /><meta name="octolytics-dimension-ga_id" content="" class="js-octo-ga-id" /><meta name="octolytics-actor-id" content="54452084" /><meta name="octolytics-actor-login" content="kezdi" /><meta name="octolytics-actor-hash" content="677bd94d587a8bf0867c76a29b1bc7618e9b3a7353a343093a229808bdc5c8fd" />

  <meta name="analytics-location" content="/&lt;user-name&gt;/&lt;repo-name&gt;/blob/show" data-pjax-transient="true" />

  





    <meta name="google-analytics" content="UA-3769691-2">

  <meta class="js-ga-set" name="userId" content="79b14dc13f19a19128af8150275bf428">

<meta class="js-ga-set" name="dimension10" content="Responsive" data-pjax-transient>

<meta class="js-ga-set" name="dimension1" content="Logged In">



  

      <meta name="hostname" content="github.com">
    <meta name="user-login" content="kezdi">


      <meta name="expected-hostname" content="github.com">

      <meta name="js-proxy-site-detection-payload" content="YmE0YmU1NWMzMTY3NGM5ZWJjYWI5ODE1MmYzZDU5NmY3ZWQ4YmMyNDcyZWNiNGIxMzZhZWQ1OWVlMWY2ZDY4N3x7InJlbW90ZV9hZGRyZXNzIjoiOTkuMTA0LjIzOS4zIiwicmVxdWVzdF9pZCI6IkM5NUU6M0ZCRTpFOTlGMjA6MTkwRERENDo1RjRDRkJENyIsInRpbWVzdGFtcCI6MTU5ODg4MDc0MCwiaG9zdCI6ImdpdGh1Yi5jb20ifQ==">

    <meta name="enabled-features" content="MARKETPLACE_PENDING_INSTALLATIONS,JS_HTTP_CACHE_HEADERS">

  <meta http-equiv="x-pjax-version" content="21265e530c51908cec368d57dd414170">
  

      <link href="https://github.com/gabors-data-analysis/da_case_studies/commits/master.atom?token=AM7N65GJ3VDJRYAIDRY3ZUN5LDVPI" rel="alternate" title="Recent Commits to da_case_studies:master" type="application/atom+xml">

  <meta name="go-import" content="github.com/gabors-data-analysis/da_case_studies git https://github.com/gabors-data-analysis/da_case_studies.git">

  <meta name="octolytics-dimension-user_id" content="42837673" /><meta name="octolytics-dimension-user_login" content="gabors-data-analysis" /><meta name="octolytics-dimension-repository_id" content="262293514" /><meta name="octolytics-dimension-repository_nwo" content="gabors-data-analysis/da_case_studies" /><meta name="octolytics-dimension-repository_public" content="false" /><meta name="octolytics-dimension-repository_is_fork" content="false" /><meta name="octolytics-dimension-repository_network_root_id" content="262293514" /><meta name="octolytics-dimension-repository_network_root_nwo" content="gabors-data-analysis/da_case_studies" /><meta name="octolytics-dimension-repository_explore_github_marketplace_ci_cta_shown" content="false" />


    <link rel="canonical" href="https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R" data-pjax-transient>


  <meta name="browser-stats-url" content="https://api.github.com/_private/browser/stats">

  <meta name="browser-errors-url" content="https://api.github.com/_private/browser/errors">

  <link rel="mask-icon" href="https://github.githubassets.com/pinned-octocat.svg" color="#000000">
  <link rel="alternate icon" class="js-site-favicon" type="image/png" href="https://github.githubassets.com/favicons/favicon.png">
  <link rel="icon" class="js-site-favicon" type="image/svg+xml" href="https://github.githubassets.com/favicons/favicon.svg">

<meta name="theme-color" content="#1e2327">


  <link rel="manifest" href="/manifest.json" crossOrigin="use-credentials">

  </head>

  <body class="logged-in env-production page-responsive page-blob">
    

    <input type="hidden" value="YVJMpx7bQZiPiMd7YlIDWi9/YaHFP5Yd4ImXN9zZNksL8xunO8sZViNHjvBw+M0elb7SGaRlB07ZFH1lJEyzJw==" data-csrf="true" class="js-data-websocket-refresh-csrf" />

    <div class="position-relative js-header-wrapper ">
      <a href="#start-of-content" class="p-3 bg-blue text-white show-on-focus js-skip-to-content">Skip to content</a>
      <span class="progress-pjax-loader width-full js-pjax-loader-bar Progress position-fixed">
    <span style="background-color: #79b8ff;width: 0%;" class="Progress-item progress-pjax-loader-bar "></span>
</span>      
      



          <header class="Header py-md-0 js-details-container Details flex-wrap flex-md-nowrap px-3" role="banner">
  <div class="Header-item d-none d-md-flex">
    <a class="Header-link" href="https://github.com/" data-hotkey="g d"
  aria-label="Homepage " data-ga-click="Header, go to dashboard, icon:logo">
  <svg class="octicon octicon-mark-github v-align-middle" height="32" viewBox="0 0 16 16" version="1.1" width="32" aria-hidden="true"><path fill-rule="evenodd" d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"></path></svg>
</a>

  </div>

  <div class="Header-item d-md-none">
    <button class="Header-link btn-link js-details-target" type="button" aria-label="Toggle navigation" aria-expanded="false">
      <svg height="24" class="octicon octicon-three-bars" viewBox="0 0 16 16" version="1.1" width="24" aria-hidden="true"><path fill-rule="evenodd" d="M1 2.75A.75.75 0 011.75 2h12.5a.75.75 0 110 1.5H1.75A.75.75 0 011 2.75zm0 5A.75.75 0 011.75 7h12.5a.75.75 0 110 1.5H1.75A.75.75 0 011 7.75zM1.75 12a.75.75 0 100 1.5h12.5a.75.75 0 100-1.5H1.75z"></path></svg>
    </button>
  </div>

  <div class="Header-item Header-item--full flex-column flex-md-row width-full flex-order-2 flex-md-order-none mr-0 mr-md-3 mt-3 mt-md-0 Details-content--hidden-not-important d-md-flex">
        <div class="header-search header-search-current js-header-search-current flex-auto  flex-self-stretch flex-md-self-auto mr-0 mr-md-3 mb-3 mb-md-0 scoped-search site-scoped-search js-site-search position-relative js-jump-to js-header-search-current-jump-to"
  role="combobox"
  aria-owns="jump-to-results"
  aria-label="Search or jump to"
  aria-haspopup="listbox"
  aria-expanded="false"
>
  <div class="position-relative">
    <!-- '"` --><!-- </textarea></xmp> --></option></form><form class="js-site-search-form" role="search" aria-label="Site" data-scope-type="Repository" data-scope-id="262293514" data-scoped-search-url="/gabors-data-analysis/da_case_studies/search" data-unscoped-search-url="/search" action="/gabors-data-analysis/da_case_studies/search" accept-charset="UTF-8" method="get">
      <label class="form-control input-sm header-search-wrapper p-0 header-search-wrapper-jump-to position-relative d-flex flex-justify-between flex-items-center js-chromeless-input-container">
        <input type="text"
          class="form-control input-sm header-search-input jump-to-field js-jump-to-field js-site-search-focus js-site-search-field is-clearable"
          data-hotkey="s,/"
          name="q"
          value=""
          placeholder="Search or jump to…"
          data-unscoped-placeholder="Search or jump to…"
          data-scoped-placeholder="Search or jump to…"
          autocapitalize="off"
          aria-autocomplete="list"
          aria-controls="jump-to-results"
          aria-label="Search or jump to…"
          data-jump-to-suggestions-path="/_graphql/GetSuggestedNavigationDestinations"
          spellcheck="false"
          autocomplete="off"
          >
          <input type="hidden" value="ctLe+5ud0J9UQjLJ2mTF+INUqQUFuhZpPn1Fv2ELWCmw5YXOrlyTeYngTeMLvrQMDKXc1FGfc+W8vJPJTIbR6g==" data-csrf="true" class="js-data-jump-to-suggestions-path-csrf" />
          <input type="hidden" class="js-site-search-type-field" name="type" >
            <img src="https://github.githubassets.com/images/search-key-slash.svg" alt="" class="mr-2 header-search-key-slash">

            <div class="Box position-absolute overflow-hidden d-none jump-to-suggestions js-jump-to-suggestions-container">
              
<ul class="d-none js-jump-to-suggestions-template-container">
  

<li class="d-flex flex-justify-start flex-items-center p-0 f5 navigation-item js-navigation-item js-jump-to-suggestion" role="option">
  <a tabindex="-1" class="no-underline d-flex flex-auto flex-items-center jump-to-suggestions-path js-jump-to-suggestion-path js-navigation-open p-2" href="">
    <div class="jump-to-octicon js-jump-to-octicon flex-shrink-0 mr-2 text-center d-none">
      <svg height="16" width="16" class="octicon octicon-repo flex-shrink-0 js-jump-to-octicon-repo d-none" title="Repository" aria-label="Repository" viewBox="0 0 16 16" version="1.1" role="img"><path fill-rule="evenodd" d="M2 2.5A2.5 2.5 0 014.5 0h8.75a.75.75 0 01.75.75v12.5a.75.75 0 01-.75.75h-2.5a.75.75 0 110-1.5h1.75v-2h-8a1 1 0 00-.714 1.7.75.75 0 01-1.072 1.05A2.495 2.495 0 012 11.5v-9zm10.5-1V9h-8c-.356 0-.694.074-1 .208V2.5a1 1 0 011-1h8zM5 12.25v3.25a.25.25 0 00.4.2l1.45-1.087a.25.25 0 01.3 0L8.6 15.7a.25.25 0 00.4-.2v-3.25a.25.25 0 00-.25-.25h-3.5a.25.25 0 00-.25.25z"></path></svg>
      <svg height="16" width="16" class="octicon octicon-project flex-shrink-0 js-jump-to-octicon-project d-none" title="Project" aria-label="Project" viewBox="0 0 16 16" version="1.1" role="img"><path fill-rule="evenodd" d="M1.75 0A1.75 1.75 0 000 1.75v12.5C0 15.216.784 16 1.75 16h12.5A1.75 1.75 0 0016 14.25V1.75A1.75 1.75 0 0014.25 0H1.75zM1.5 1.75a.25.25 0 01.25-.25h12.5a.25.25 0 01.25.25v12.5a.25.25 0 01-.25.25H1.75a.25.25 0 01-.25-.25V1.75zM11.75 3a.75.75 0 00-.75.75v7.5a.75.75 0 001.5 0v-7.5a.75.75 0 00-.75-.75zm-8.25.75a.75.75 0 011.5 0v5.5a.75.75 0 01-1.5 0v-5.5zM8 3a.75.75 0 00-.75.75v3.5a.75.75 0 001.5 0v-3.5A.75.75 0 008 3z"></path></svg>
      <svg height="16" width="16" class="octicon octicon-search flex-shrink-0 js-jump-to-octicon-search d-none" title="Search" aria-label="Search" viewBox="0 0 16 16" version="1.1" role="img"><path fill-rule="evenodd" d="M11.5 7a4.499 4.499 0 11-8.998 0A4.499 4.499 0 0111.5 7zm-.82 4.74a6 6 0 111.06-1.06l3.04 3.04a.75.75 0 11-1.06 1.06l-3.04-3.04z"></path></svg>
    </div>

    <img class="avatar mr-2 flex-shrink-0 js-jump-to-suggestion-avatar d-none" alt="" aria-label="Team" src="" width="28" height="28">

    <div class="jump-to-suggestion-name js-jump-to-suggestion-name flex-auto overflow-hidden text-left no-wrap css-truncate css-truncate-target">
    </div>

    <div class="border rounded-1 flex-shrink-0 bg-gray px-1 text-gray-light ml-1 f6 d-none js-jump-to-badge-search">
      <span class="js-jump-to-badge-search-text-default d-none" aria-label="in this repository">
        In this repository
      </span>
      <span class="js-jump-to-badge-search-text-global d-none" aria-label="in all of GitHub">
        All GitHub
      </span>
      <span aria-hidden="true" class="d-inline-block ml-1 v-align-middle">↵</span>
    </div>

    <div aria-hidden="true" class="border rounded-1 flex-shrink-0 bg-gray px-1 text-gray-light ml-1 f6 d-none d-on-nav-focus js-jump-to-badge-jump">
      Jump to
      <span class="d-inline-block ml-1 v-align-middle">↵</span>
    </div>
  </a>
</li>

</ul>

<ul class="d-none js-jump-to-no-results-template-container">
  <li class="d-flex flex-justify-center flex-items-center f5 d-none js-jump-to-suggestion p-2">
    <span class="text-gray">No suggested jump to results</span>
  </li>
</ul>

<ul id="jump-to-results" role="listbox" class="p-0 m-0 js-navigation-container jump-to-suggestions-results-container js-jump-to-suggestions-results-container">
  

<li class="d-flex flex-justify-start flex-items-center p-0 f5 navigation-item js-navigation-item js-jump-to-scoped-search d-none" role="option">
  <a tabindex="-1" class="no-underline d-flex flex-auto flex-items-center jump-to-suggestions-path js-jump-to-suggestion-path js-navigation-open p-2" href="">
    <div class="jump-to-octicon js-jump-to-octicon flex-shrink-0 mr-2 text-center d-none">
      <svg height="16" width="16" class="octicon octicon-repo flex-shrink-0 js-jump-to-octicon-repo d-none" title="Repository" aria-label="Repository" viewBox="0 0 16 16" version="1.1" role="img"><path fill-rule="evenodd" d="M2 2.5A2.5 2.5 0 014.5 0h8.75a.75.75 0 01.75.75v12.5a.75.75 0 01-.75.75h-2.5a.75.75 0 110-1.5h1.75v-2h-8a1 1 0 00-.714 1.7.75.75 0 01-1.072 1.05A2.495 2.495 0 012 11.5v-9zm10.5-1V9h-8c-.356 0-.694.074-1 .208V2.5a1 1 0 011-1h8zM5 12.25v3.25a.25.25 0 00.4.2l1.45-1.087a.25.25 0 01.3 0L8.6 15.7a.25.25 0 00.4-.2v-3.25a.25.25 0 00-.25-.25h-3.5a.25.25 0 00-.25.25z"></path></svg>
      <svg height="16" width="16" class="octicon octicon-project flex-shrink-0 js-jump-to-octicon-project d-none" title="Project" aria-label="Project" viewBox="0 0 16 16" version="1.1" role="img"><path fill-rule="evenodd" d="M1.75 0A1.75 1.75 0 000 1.75v12.5C0 15.216.784 16 1.75 16h12.5A1.75 1.75 0 0016 14.25V1.75A1.75 1.75 0 0014.25 0H1.75zM1.5 1.75a.25.25 0 01.25-.25h12.5a.25.25 0 01.25.25v12.5a.25.25 0 01-.25.25H1.75a.25.25 0 01-.25-.25V1.75zM11.75 3a.75.75 0 00-.75.75v7.5a.75.75 0 001.5 0v-7.5a.75.75 0 00-.75-.75zm-8.25.75a.75.75 0 011.5 0v5.5a.75.75 0 01-1.5 0v-5.5zM8 3a.75.75 0 00-.75.75v3.5a.75.75 0 001.5 0v-3.5A.75.75 0 008 3z"></path></svg>
      <svg height="16" width="16" class="octicon octicon-search flex-shrink-0 js-jump-to-octicon-search d-none" title="Search" aria-label="Search" viewBox="0 0 16 16" version="1.1" role="img"><path fill-rule="evenodd" d="M11.5 7a4.499 4.499 0 11-8.998 0A4.499 4.499 0 0111.5 7zm-.82 4.74a6 6 0 111.06-1.06l3.04 3.04a.75.75 0 11-1.06 1.06l-3.04-3.04z"></path></svg>
    </div>

    <img class="avatar mr-2 flex-shrink-0 js-jump-to-suggestion-avatar d-none" alt="" aria-label="Team" src="" width="28" height="28">

    <div class="jump-to-suggestion-name js-jump-to-suggestion-name flex-auto overflow-hidden text-left no-wrap css-truncate css-truncate-target">
    </div>

    <div class="border rounded-1 flex-shrink-0 bg-gray px-1 text-gray-light ml-1 f6 d-none js-jump-to-badge-search">
      <span class="js-jump-to-badge-search-text-default d-none" aria-label="in this repository">
        In this repository
      </span>
      <span class="js-jump-to-badge-search-text-global d-none" aria-label="in all of GitHub">
        All GitHub
      </span>
      <span aria-hidden="true" class="d-inline-block ml-1 v-align-middle">↵</span>
    </div>

    <div aria-hidden="true" class="border rounded-1 flex-shrink-0 bg-gray px-1 text-gray-light ml-1 f6 d-none d-on-nav-focus js-jump-to-badge-jump">
      Jump to
      <span class="d-inline-block ml-1 v-align-middle">↵</span>
    </div>
  </a>
</li>

  

<li class="d-flex flex-justify-start flex-items-center p-0 f5 navigation-item js-navigation-item js-jump-to-global-search d-none" role="option">
  <a tabindex="-1" class="no-underline d-flex flex-auto flex-items-center jump-to-suggestions-path js-jump-to-suggestion-path js-navigation-open p-2" href="">
    <div class="jump-to-octicon js-jump-to-octicon flex-shrink-0 mr-2 text-center d-none">
      <svg height="16" width="16" class="octicon octicon-repo flex-shrink-0 js-jump-to-octicon-repo d-none" title="Repository" aria-label="Repository" viewBox="0 0 16 16" version="1.1" role="img"><path fill-rule="evenodd" d="M2 2.5A2.5 2.5 0 014.5 0h8.75a.75.75 0 01.75.75v12.5a.75.75 0 01-.75.75h-2.5a.75.75 0 110-1.5h1.75v-2h-8a1 1 0 00-.714 1.7.75.75 0 01-1.072 1.05A2.495 2.495 0 012 11.5v-9zm10.5-1V9h-8c-.356 0-.694.074-1 .208V2.5a1 1 0 011-1h8zM5 12.25v3.25a.25.25 0 00.4.2l1.45-1.087a.25.25 0 01.3 0L8.6 15.7a.25.25 0 00.4-.2v-3.25a.25.25 0 00-.25-.25h-3.5a.25.25 0 00-.25.25z"></path></svg>
      <svg height="16" width="16" class="octicon octicon-project flex-shrink-0 js-jump-to-octicon-project d-none" title="Project" aria-label="Project" viewBox="0 0 16 16" version="1.1" role="img"><path fill-rule="evenodd" d="M1.75 0A1.75 1.75 0 000 1.75v12.5C0 15.216.784 16 1.75 16h12.5A1.75 1.75 0 0016 14.25V1.75A1.75 1.75 0 0014.25 0H1.75zM1.5 1.75a.25.25 0 01.25-.25h12.5a.25.25 0 01.25.25v12.5a.25.25 0 01-.25.25H1.75a.25.25 0 01-.25-.25V1.75zM11.75 3a.75.75 0 00-.75.75v7.5a.75.75 0 001.5 0v-7.5a.75.75 0 00-.75-.75zm-8.25.75a.75.75 0 011.5 0v5.5a.75.75 0 01-1.5 0v-5.5zM8 3a.75.75 0 00-.75.75v3.5a.75.75 0 001.5 0v-3.5A.75.75 0 008 3z"></path></svg>
      <svg height="16" width="16" class="octicon octicon-search flex-shrink-0 js-jump-to-octicon-search d-none" title="Search" aria-label="Search" viewBox="0 0 16 16" version="1.1" role="img"><path fill-rule="evenodd" d="M11.5 7a4.499 4.499 0 11-8.998 0A4.499 4.499 0 0111.5 7zm-.82 4.74a6 6 0 111.06-1.06l3.04 3.04a.75.75 0 11-1.06 1.06l-3.04-3.04z"></path></svg>
    </div>

    <img class="avatar mr-2 flex-shrink-0 js-jump-to-suggestion-avatar d-none" alt="" aria-label="Team" src="" width="28" height="28">

    <div class="jump-to-suggestion-name js-jump-to-suggestion-name flex-auto overflow-hidden text-left no-wrap css-truncate css-truncate-target">
    </div>

    <div class="border rounded-1 flex-shrink-0 bg-gray px-1 text-gray-light ml-1 f6 d-none js-jump-to-badge-search">
      <span class="js-jump-to-badge-search-text-default d-none" aria-label="in this repository">
        In this repository
      </span>
      <span class="js-jump-to-badge-search-text-global d-none" aria-label="in all of GitHub">
        All GitHub
      </span>
      <span aria-hidden="true" class="d-inline-block ml-1 v-align-middle">↵</span>
    </div>

    <div aria-hidden="true" class="border rounded-1 flex-shrink-0 bg-gray px-1 text-gray-light ml-1 f6 d-none d-on-nav-focus js-jump-to-badge-jump">
      Jump to
      <span class="d-inline-block ml-1 v-align-middle">↵</span>
    </div>
  </a>
</li>


    <li class="d-flex flex-justify-center flex-items-center p-0 f5 js-jump-to-suggestion">
      <img src="https://github.githubassets.com/images/spinners/octocat-spinner-128.gif" alt="Octocat Spinner Icon" class="m-2" width="28">
    </li>
</ul>

            </div>
      </label>
</form>  </div>
</div>


    <nav class="d-flex flex-column flex-md-row flex-self-stretch flex-md-self-auto" aria-label="Global">
    <a class="Header-link py-md-3 d-block d-md-none py-2 border-top border-md-top-0 border-white-fade-15" data-ga-click="Header, click, Nav menu - item:dashboard:user" aria-label="Dashboard" href="/dashboard">
      Dashboard
</a>
  <a class="js-selected-navigation-item Header-link py-md-3  mr-0 mr-md-3 py-2 border-top border-md-top-0 border-white-fade-15" data-hotkey="g p" data-ga-click="Header, click, Nav menu - item:pulls context:user" aria-label="Pull requests you created" data-selected-links="/pulls /pulls/assigned /pulls/mentioned /pulls" href="/pulls">
      Pull<span class="d-inline d-md-none d-lg-inline"> request</span>s
</a>
  <a class="js-selected-navigation-item Header-link py-md-3  mr-0 mr-md-3 py-2 border-top border-md-top-0 border-white-fade-15" data-hotkey="g i" data-ga-click="Header, click, Nav menu - item:issues context:user" aria-label="Issues you created" data-selected-links="/issues /issues/assigned /issues/mentioned /issues" href="/issues">
    Issues
</a>

    <div class="mr-0 mr-md-3 py-2 py-md-0 border-top border-md-top-0 border-white-fade-15">
      <a class="js-selected-navigation-item Header-link py-md-3 d-inline-block" data-ga-click="Header, click, Nav menu - item:marketplace context:user" data-octo-click="marketplace_click" data-octo-dimensions="location:nav_bar" data-selected-links=" /marketplace" href="/marketplace">
        Marketplace
</a>      

    </div>

  <a class="js-selected-navigation-item Header-link py-md-3  mr-0 mr-md-3 py-2 border-top border-md-top-0 border-white-fade-15" data-ga-click="Header, click, Nav menu - item:explore" data-selected-links="/explore /trending /trending/developers /integrations /integrations/feature/code /integrations/feature/collaborate /integrations/feature/ship showcases showcases_search showcases_landing /explore" href="/explore">
    Explore
</a>


    <a class="Header-link d-block d-md-none mr-0 mr-md-3 py-2 py-md-3 border-top border-md-top-0 border-white-fade-15" href="/kezdi">
      <img class="avatar avatar-user" src="https://avatars0.githubusercontent.com/u/54452084?s=40&amp;v=4" width="20" height="20" alt="@kezdi" />
      kezdi
</a>
    <!-- '"` --><!-- </textarea></xmp> --></option></form><form action="/logout" accept-charset="UTF-8" method="post"><input type="hidden" name="authenticity_token" value="y2PVNkLAa/L8REAkamdUVkyzlvTe2iWXyaoA+ln0E7pJYhBMin3Hta2TybraiOKe0PYaAwBiEJshU0YykGMiXw==" />
      <button type="submit" class="Header-link mr-0 mr-md-3 py-2 py-md-3 border-top border-md-top-0 border-white-fade-15 d-md-none btn-link d-block width-full text-left" data-ga-click="Header, sign out, icon:logout" style="padding-left: 2px;">
        <svg class="octicon octicon-sign-out v-align-middle" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M2 2.75C2 1.784 2.784 1 3.75 1h2.5a.75.75 0 010 1.5h-2.5a.25.25 0 00-.25.25v10.5c0 .138.112.25.25.25h2.5a.75.75 0 010 1.5h-2.5A1.75 1.75 0 012 13.25V2.75zm10.44 4.5H6.75a.75.75 0 000 1.5h5.69l-1.97 1.97a.75.75 0 101.06 1.06l3.25-3.25a.75.75 0 000-1.06l-3.25-3.25a.75.75 0 10-1.06 1.06l1.97 1.97z"></path></svg>
        Sign out
      </button>
</form></nav>

  </div>

  <div class="Header-item Header-item--full flex-justify-center d-md-none position-relative">
    <a class="Header-link" href="https://github.com/" data-hotkey="g d"
  aria-label="Homepage " data-ga-click="Header, go to dashboard, icon:logo">
  <svg class="octicon octicon-mark-github v-align-middle" height="32" viewBox="0 0 16 16" version="1.1" width="32" aria-hidden="true"><path fill-rule="evenodd" d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"></path></svg>
</a>

  </div>

  <div class="Header-item mr-0 mr-md-3 flex-order-1 flex-md-order-none">
    

    <notification-indicator class="js-socket-channel" data-channel="eyJjIjoibm90aWZpY2F0aW9uLWNoYW5nZWQ6NTQ0NTIwODQiLCJ0IjoxNTk4ODgwNzQwfQ==--8e00dceb80654f357da1d4920d94173f4faea914c3ffeb3b8c39f6016e447809">
      <a href="/notifications"
         class="Header-link notification-indicator position-relative tooltipped tooltipped-sw"
         aria-label="You have no unread notifications"
         data-hotkey="g n"
         data-ga-click="Header, go to notifications, icon:read"
         data-target="notification-indicator.link">
         <span class="mail-status " data-target="notification-indicator.modifier"></span>
         <svg class="octicon octicon-bell" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path d="M8 16a2 2 0 001.985-1.75c.017-.137-.097-.25-.235-.25h-3.5c-.138 0-.252.113-.235.25A2 2 0 008 16z"></path><path fill-rule="evenodd" d="M8 1.5A3.5 3.5 0 004.5 5v2.947c0 .346-.102.683-.294.97l-1.703 2.556a.018.018 0 00-.003.01l.001.006c0 .002.002.004.004.006a.017.017 0 00.006.004l.007.001h10.964l.007-.001a.016.016 0 00.006-.004.016.016 0 00.004-.006l.001-.007a.017.017 0 00-.003-.01l-1.703-2.554a1.75 1.75 0 01-.294-.97V5A3.5 3.5 0 008 1.5zM3 5a5 5 0 0110 0v2.947c0 .05.015.098.042.139l1.703 2.555A1.518 1.518 0 0113.482 13H2.518a1.518 1.518 0 01-1.263-2.36l1.703-2.554A.25.25 0 003 7.947V5z"></path></svg>
      </a>
    </notification-indicator>

  </div>


  <div class="Header-item position-relative d-none d-md-flex">
    <details class="details-overlay details-reset">
  <summary class="Header-link"
      aria-label="Create new…"
      data-ga-click="Header, create new, icon:add">
    <svg class="octicon octicon-plus" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M8 2a.75.75 0 01.75.75v4.5h4.5a.75.75 0 010 1.5h-4.5v4.5a.75.75 0 01-1.5 0v-4.5h-4.5a.75.75 0 010-1.5h4.5v-4.5A.75.75 0 018 2z"></path></svg> <span class="dropdown-caret"></span>
  </summary>
  <details-menu class="dropdown-menu dropdown-menu-sw mt-n2">
    
<a role="menuitem" class="dropdown-item" href="/new" data-ga-click="Header, create new repository">
  New repository
</a>

  <a role="menuitem" class="dropdown-item" href="/new/import" data-ga-click="Header, import a repository">
    Import repository
  </a>

<a role="menuitem" class="dropdown-item" href="https://gist.github.com/" data-ga-click="Header, create new gist">
  New gist
</a>

  <a role="menuitem" class="dropdown-item" href="/organizations/new" data-ga-click="Header, create new organization">
    New organization
  </a>


  <div role="none" class="dropdown-divider"></div>
  <div class="dropdown-header">
    <span title="gabors-data-analysis/da_case_studies">This repository</span>
  </div>
    <a role="menuitem" class="dropdown-item" href="/gabors-data-analysis/da_case_studies/issues/new/choose" data-ga-click="Header, create new issue" data-skip-pjax>
      New issue
    </a>


  </details-menu>
</details>

  </div>

  <div class="Header-item position-relative mr-0 d-none d-md-flex">
    
  <details class="details-overlay details-reset js-feature-preview-indicator-container" data-feature-preview-indicator-src="/users/kezdi/feature_preview/indicator_check">

  <summary class="Header-link"
    aria-label="View profile and more"
    data-ga-click="Header, show menu, icon:avatar">
    <img
  alt="@kezdi"
  width="20"
  height="20"
  src="https://avatars3.githubusercontent.com/u/54452084?s=60&amp;v=4"
  class="avatar avatar-user " />

      <span class="feature-preview-indicator js-feature-preview-indicator" style="top: 10px;" hidden></span>
    <span class="dropdown-caret"></span>
  </summary>
  <details-menu class="dropdown-menu dropdown-menu-sw mt-n2" style="width: 180px" >
    <div class="header-nav-current-user css-truncate"><a role="menuitem" class="no-underline user-profile-link px-3 pt-2 pb-2 mb-n2 mt-n1 d-block" href="/kezdi" data-ga-click="Header, go to profile, text:Signed in as">Signed in as <strong class="css-truncate-target">kezdi</strong></a></div>
    <div role="none" class="dropdown-divider"></div>

      <div class="pl-3 pr-3 f6 user-status-container js-user-status-context lh-condensed" data-url="/users/status?compact=1&amp;link_mentions=0&amp;truncate=1">
        
<div class="js-user-status-container rounded-1 px-2 py-1 mt-2 border"
  data-team-hovercards-enabled>
  <details class="js-user-status-details details-reset details-overlay details-overlay-dark">
    <summary class="btn-link btn-block link-gray no-underline js-toggle-user-status-edit toggle-user-status-edit "
      role="menuitem" data-hydro-click="{&quot;event_type&quot;:&quot;user_profile.click&quot;,&quot;payload&quot;:{&quot;profile_user_id&quot;:42837673,&quot;target&quot;:&quot;EDIT_USER_STATUS&quot;,&quot;user_id&quot;:54452084,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;}}" data-hydro-click-hmac="b58a4dc501f1a6541ef240998faff5206c0f3bd13cf8ab1bdf2f0d243259dd5e">
      <div class="d-flex flex-items-center flex-items-stretch">
        <div class="f6 lh-condensed user-status-header d-flex user-status-emoji-only-header circle">
          <div class="user-status-emoji-container flex-shrink-0 mr-2 d-flex flex-items-center flex-justify-center lh-condensed-ultra v-align-bottom">
            <svg class="octicon octicon-smiley" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M1.5 8a6.5 6.5 0 1113 0 6.5 6.5 0 01-13 0zM8 0a8 8 0 100 16A8 8 0 008 0zM5 8a1 1 0 100-2 1 1 0 000 2zm7-1a1 1 0 11-2 0 1 1 0 012 0zM5.32 9.636a.75.75 0 011.038.175l.007.009c.103.118.22.222.35.31.264.178.683.37 1.285.37.602 0 1.02-.192 1.285-.371.13-.088.247-.192.35-.31l.007-.008a.75.75 0 111.222.87l-.614-.431c.614.43.614.431.613.431v.001l-.001.002-.002.003-.005.007-.014.019a1.984 1.984 0 01-.184.213c-.16.166-.338.316-.53.445-.63.418-1.37.638-2.127.629-.946 0-1.652-.308-2.126-.63a3.32 3.32 0 01-.715-.657l-.014-.02-.005-.006-.002-.003v-.002h-.001l.613-.432-.614.43a.75.75 0 01.183-1.044h.001z"></path></svg>
          </div>
        </div>
        <div class="
          
           user-status-message-wrapper f6 min-width-0"
           style="line-height: 20px;" >
          <div class="css-truncate css-truncate-target width-fit text-gray-dark text-left">
              <span class="text-gray">Set status</span>
          </div>
        </div>
      </div>
    </summary>
    <details-dialog class="details-dialog rounded-1 anim-fade-in fast Box Box--overlay" role="dialog" tabindex="-1">
      <!-- '"` --><!-- </textarea></xmp> --></option></form><form class="position-relative flex-auto js-user-status-form" action="/users/status?circle=0&amp;compact=1&amp;link_mentions=0&amp;truncate=1" accept-charset="UTF-8" method="post"><input type="hidden" name="_method" value="put" /><input type="hidden" name="authenticity_token" value="X7pz4hh7iMxQbq2QforAOVgLt5uHUBJQVZ4b/jfl6cCB2zTnWh5VZ6l1rSqkplXd40DxJJR15MZR8cGeRwktzQ==" />
        <div class="Box-header bg-gray border-bottom p-3">
          <button class="Box-btn-octicon js-toggle-user-status-edit btn-octicon float-right" type="reset" aria-label="Close dialog" data-close-dialog>
            <svg class="octicon octicon-x" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M3.72 3.72a.75.75 0 011.06 0L8 6.94l3.22-3.22a.75.75 0 111.06 1.06L9.06 8l3.22 3.22a.75.75 0 11-1.06 1.06L8 9.06l-3.22 3.22a.75.75 0 01-1.06-1.06L6.94 8 3.72 4.78a.75.75 0 010-1.06z"></path></svg>
          </button>
          <h3 class="Box-title f5 text-bold text-gray-dark">Edit status</h3>
        </div>
        <input type="hidden" name="emoji" class="js-user-status-emoji-field" value="">
        <input type="hidden" name="organization_id" class="js-user-status-org-id-field" value="">
        <div class="px-3 py-2 text-gray-dark">
          <div class="js-characters-remaining-container position-relative mt-2">
            <div class="input-group d-table form-group my-0 js-user-status-form-group">
              <span class="input-group-button d-table-cell v-align-middle" style="width: 1%">
                <button type="button" aria-label="Choose an emoji" class="btn-outline btn js-toggle-user-status-emoji-picker btn-open-emoji-picker p-0">
                  <span class="js-user-status-original-emoji" hidden></span>
                  <span class="js-user-status-custom-emoji"></span>
                  <span class="js-user-status-no-emoji-icon" >
                    <svg class="octicon octicon-smiley" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M1.5 8a6.5 6.5 0 1113 0 6.5 6.5 0 01-13 0zM8 0a8 8 0 100 16A8 8 0 008 0zM5 8a1 1 0 100-2 1 1 0 000 2zm7-1a1 1 0 11-2 0 1 1 0 012 0zM5.32 9.636a.75.75 0 011.038.175l.007.009c.103.118.22.222.35.31.264.178.683.37 1.285.37.602 0 1.02-.192 1.285-.371.13-.088.247-.192.35-.31l.007-.008a.75.75 0 111.222.87l-.614-.431c.614.43.614.431.613.431v.001l-.001.002-.002.003-.005.007-.014.019a1.984 1.984 0 01-.184.213c-.16.166-.338.316-.53.445-.63.418-1.37.638-2.127.629-.946 0-1.652-.308-2.126-.63a3.32 3.32 0 01-.715-.657l-.014-.02-.005-.006-.002-.003v-.002h-.001l.613-.432-.614.43a.75.75 0 01.183-1.044h.001z"></path></svg>
                  </span>
                </button>
              </span>
              <text-expander keys=": @" data-mention-url="/autocomplete/user-suggestions" data-emoji-url="/autocomplete/emoji">
                <input
                  type="text"
                  autocomplete="off"
                  data-no-org-url="/autocomplete/user-suggestions"
                  data-org-url="/suggestions?mention_suggester=1"
                  data-maxlength="80"
                  class="d-table-cell width-full form-control js-user-status-message-field js-characters-remaining-field"
                  placeholder="What's happening?"
                  name="message"
                  value=""
                  aria-label="What is your current status?">
              </text-expander>
              <div class="error">Could not update your status, please try again.</div>
            </div>
            <div style="margin-left: 53px" class="my-1 text-small label-characters-remaining js-characters-remaining" data-suffix="remaining" hidden>
              80 remaining
            </div>
          </div>
          <include-fragment class="js-user-status-emoji-picker" data-url="/users/status/emoji"></include-fragment>
          <div class="overflow-auto ml-n3 mr-n3 px-3 border-bottom" style="max-height: 33vh">
            <div class="user-status-suggestions js-user-status-suggestions collapsed overflow-hidden">
              <h4 class="f6 text-normal my-3">Suggestions:</h4>
              <div class="mx-3 mt-2 clearfix">
                  <div class="float-left col-6">
                      <button type="button" value=":palm_tree:" class="d-flex flex-items-baseline flex-items-stretch lh-condensed f6 btn-link link-gray no-underline js-predefined-user-status mb-1">
                        <div class="emoji-status-width mr-2 v-align-middle js-predefined-user-status-emoji">
                          <g-emoji alias="palm_tree" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/1f334.png">🌴</g-emoji>
                        </div>
                        <div class="d-flex flex-items-center no-underline js-predefined-user-status-message ws-normal text-left" style="border-left: 1px solid transparent">
                          On vacation
                        </div>
                      </button>
                      <button type="button" value=":face_with_thermometer:" class="d-flex flex-items-baseline flex-items-stretch lh-condensed f6 btn-link link-gray no-underline js-predefined-user-status mb-1">
                        <div class="emoji-status-width mr-2 v-align-middle js-predefined-user-status-emoji">
                          <g-emoji alias="face_with_thermometer" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/1f912.png">🤒</g-emoji>
                        </div>
                        <div class="d-flex flex-items-center no-underline js-predefined-user-status-message ws-normal text-left" style="border-left: 1px solid transparent">
                          Out sick
                        </div>
                      </button>
                  </div>
                  <div class="float-left col-6">
                      <button type="button" value=":house:" class="d-flex flex-items-baseline flex-items-stretch lh-condensed f6 btn-link link-gray no-underline js-predefined-user-status mb-1">
                        <div class="emoji-status-width mr-2 v-align-middle js-predefined-user-status-emoji">
                          <g-emoji alias="house" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/1f3e0.png">🏠</g-emoji>
                        </div>
                        <div class="d-flex flex-items-center no-underline js-predefined-user-status-message ws-normal text-left" style="border-left: 1px solid transparent">
                          Working from home
                        </div>
                      </button>
                      <button type="button" value=":dart:" class="d-flex flex-items-baseline flex-items-stretch lh-condensed f6 btn-link link-gray no-underline js-predefined-user-status mb-1">
                        <div class="emoji-status-width mr-2 v-align-middle js-predefined-user-status-emoji">
                          <g-emoji alias="dart" fallback-src="https://github.githubassets.com/images/icons/emoji/unicode/1f3af.png">🎯</g-emoji>
                        </div>
                        <div class="d-flex flex-items-center no-underline js-predefined-user-status-message ws-normal text-left" style="border-left: 1px solid transparent">
                          Focusing
                        </div>
                      </button>
                  </div>
              </div>
            </div>
            <div class="user-status-limited-availability-container">
              <div class="form-checkbox my-0">
                <input type="checkbox" name="limited_availability" value="1" class="js-user-status-limited-availability-checkbox" data-default-message="I may be slow to respond." aria-describedby="limited-availability-help-text-truncate-true-compact-true" id="limited-availability-truncate-true-compact-true">
                <label class="d-block f5 text-gray-dark mb-1" for="limited-availability-truncate-true-compact-true">
                  Busy
                </label>
                <p class="note" id="limited-availability-help-text-truncate-true-compact-true">
                  When others mention you, assign you, or request your review,
                  GitHub will let them know that you have limited availability.
                </p>
              </div>
            </div>
          </div>
          <div class="d-inline-block f5 mr-2 pt-3 pb-2" >
  <div class="d-inline-block mr-1">
    Clear status
  </div>

  <details class="js-user-status-expire-drop-down f6 dropdown details-reset details-overlay d-inline-block mr-2">
    <summary class="btn btn-sm v-align-baseline" aria-haspopup="true">
      <div class="js-user-status-expiration-interval-selected d-inline-block v-align-baseline">
        Never
      </div>
      <div class="dropdown-caret"></div>
    </summary>

    <ul class="dropdown-menu dropdown-menu-se pl-0 overflow-auto" style="width: 220px; max-height: 15.5em">
      <li>
        <button type="button" class="btn-link dropdown-item js-user-status-expire-button ws-normal" title="Never">
          <span class="d-inline-block text-bold mb-1">Never</span>
          <div class="f6 lh-condensed">Keep this status until you clear your status or edit your status.</div>
        </button>
      </li>
      <li class="dropdown-divider" role="none"></li>
        <li>
          <button type="button" class="btn-link dropdown-item ws-normal js-user-status-expire-button" title="in 30 minutes" value="2020-08-31T10:02:20-04:00">
            in 30 minutes
          </button>
        </li>
        <li>
          <button type="button" class="btn-link dropdown-item ws-normal js-user-status-expire-button" title="in 1 hour" value="2020-08-31T10:32:20-04:00">
            in 1 hour
          </button>
        </li>
        <li>
          <button type="button" class="btn-link dropdown-item ws-normal js-user-status-expire-button" title="in 4 hours" value="2020-08-31T13:32:20-04:00">
            in 4 hours
          </button>
        </li>
        <li>
          <button type="button" class="btn-link dropdown-item ws-normal js-user-status-expire-button" title="today" value="2020-08-31T23:59:59-04:00">
            today
          </button>
        </li>
        <li>
          <button type="button" class="btn-link dropdown-item ws-normal js-user-status-expire-button" title="this week" value="2020-09-06T23:59:59-04:00">
            this week
          </button>
        </li>
    </ul>
  </details>
  <input class="js-user-status-expiration-date-input" type="hidden" name="expires_at" value="">
</div>

          <include-fragment class="js-user-status-org-picker" data-url="/users/status/organizations"></include-fragment>
        </div>
        <div class="d-flex flex-items-center flex-justify-between p-3 border-top">
          <button type="submit" disabled class="width-full btn btn-primary mr-2 js-user-status-submit">
            Set status
          </button>
          <button type="button" disabled class="width-full js-clear-user-status-button btn ml-2 ">
            Clear status
          </button>
        </div>
</form>    </details-dialog>
  </details>
</div>

      </div>
      <div role="none" class="dropdown-divider"></div>

    <a role="menuitem" class="dropdown-item" href="/kezdi" data-ga-click="Header, go to profile, text:your profile" data-hydro-click="{&quot;event_type&quot;:&quot;global_header.user_menu_dropdown.click&quot;,&quot;payload&quot;:{&quot;request_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;target&quot;:&quot;YOUR_PROFILE&quot;,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}" data-hydro-click-hmac="5d71e86a7753927c1199790b0bf61f7b719573640dc449a5aef7f83ce44b93a8" >Your profile</a>

    <a role="menuitem" class="dropdown-item" href="/kezdi?tab=repositories" data-ga-click="Header, go to repositories, text:your repositories" data-hydro-click="{&quot;event_type&quot;:&quot;global_header.user_menu_dropdown.click&quot;,&quot;payload&quot;:{&quot;request_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;target&quot;:&quot;YOUR_REPOSITORIES&quot;,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}" data-hydro-click-hmac="3b3a7098098ae729935a30ca56df5ac13bc71a23adf75032e64652e2c8c92d40" >Your repositories</a>

      <a role="menuitem"
         class="dropdown-item"
         href="/settings/organizations"
         
         data-ga-click="Header, go to organizations, text:your organizations"
         data-hydro-click="{&quot;event_type&quot;:&quot;global_header.user_menu_dropdown.click&quot;,&quot;payload&quot;:{&quot;request_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;target&quot;:&quot;YOUR_ORGANIZATIONS&quot;,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}" data-hydro-click-hmac="bb93bbeb74b81005bbc8ff1d6eaaea615649caee26266520427d5678a0ece11c"
         >Your organizations</a>


    <a role="menuitem" class="dropdown-item" href="/kezdi?tab=projects" data-ga-click="Header, go to projects, text:your projects" data-hydro-click="{&quot;event_type&quot;:&quot;global_header.user_menu_dropdown.click&quot;,&quot;payload&quot;:{&quot;request_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;target&quot;:&quot;YOUR_PROJECTS&quot;,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}" data-hydro-click-hmac="54cfac1aec8c678672d544d451fbe07ec7f85ced6ae8a946216f234a6dfd986a" >Your projects</a>


    <a role="menuitem" class="dropdown-item" href="/kezdi?tab=stars" data-ga-click="Header, go to starred repos, text:your stars" data-hydro-click="{&quot;event_type&quot;:&quot;global_header.user_menu_dropdown.click&quot;,&quot;payload&quot;:{&quot;request_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;target&quot;:&quot;YOUR_STARS&quot;,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}" data-hydro-click-hmac="dc073032b6f205fbd427b864e1784ce9d6fe47d34e3dc9f0ef01e7eb82331029" >Your stars</a>
      <a role="menuitem" class="dropdown-item" href="https://gist.github.com/mine" data-ga-click="Header, your gists, text:your gists" data-hydro-click="{&quot;event_type&quot;:&quot;global_header.user_menu_dropdown.click&quot;,&quot;payload&quot;:{&quot;request_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;target&quot;:&quot;YOUR_GISTS&quot;,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}" data-hydro-click-hmac="8c254ab23f4b9cd92348cba91a72c7947751179e21a2e13e18d77e37208f6753" >Your gists</a>





    <div role="none" class="dropdown-divider"></div>
      <a role="menuitem" class="dropdown-item" href="/settings/billing" data-ga-click="Header, go to billing, text:upgrade" data-hydro-click="{&quot;event_type&quot;:&quot;global_header.user_menu_dropdown.click&quot;,&quot;payload&quot;:{&quot;request_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;target&quot;:&quot;UPGRADE&quot;,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}" data-hydro-click-hmac="a48a2c97667d0ceaac273b4e35f9cf93909de5a353c412d7366ed56a1662c4af" >Upgrade</a>
      
<div id="feature-enrollment-toggle" class="hide-sm hide-md feature-preview-details position-relative">
  <button
    type="button"
    class="dropdown-item btn-link"
    role="menuitem"
    data-feature-preview-trigger-url="/users/kezdi/feature_previews"
    data-feature-preview-close-details="{&quot;event_type&quot;:&quot;feature_preview.clicks.close_modal&quot;,&quot;payload&quot;:{&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}"
    data-feature-preview-close-hmac="8363536c4ba92e5de1f96d9f5ef7ddc59d9c2496e69422d75ac3840dc022c1a7"
    data-hydro-click="{&quot;event_type&quot;:&quot;feature_preview.clicks.open_modal&quot;,&quot;payload&quot;:{&quot;link_location&quot;:&quot;user_dropdown&quot;,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}"
    data-hydro-click-hmac="417721a8e24885eaddb359fe14ca743c11c6878a90e6f4f02f1cb63ef7518180"
  >
    Feature preview
  </button>
    <span class="feature-preview-indicator js-feature-preview-indicator" hidden></span>
</div>

    <a role="menuitem" class="dropdown-item" href="https://docs.github.com" data-ga-click="Header, go to help, text:help" data-hydro-click="{&quot;event_type&quot;:&quot;global_header.user_menu_dropdown.click&quot;,&quot;payload&quot;:{&quot;request_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;target&quot;:&quot;HELP&quot;,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}" data-hydro-click-hmac="8675c8ac109fe14a73abb0ea503d4988e832efafda9d596af507eee25a33a328" >Help</a>
    <a role="menuitem" class="dropdown-item" href="/settings/profile" data-ga-click="Header, go to settings, icon:settings" data-hydro-click="{&quot;event_type&quot;:&quot;global_header.user_menu_dropdown.click&quot;,&quot;payload&quot;:{&quot;request_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;target&quot;:&quot;SETTINGS&quot;,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}" data-hydro-click-hmac="cd8feaf6350f87725a4f137526ddeca279ea14df34574f2e0a30ada8a94682f3" >Settings</a>
    <!-- '"` --><!-- </textarea></xmp> --></option></form><form class="logout-form" action="/logout" accept-charset="UTF-8" method="post"><input type="hidden" name="authenticity_token" value="uuEy9KJZG85MLGg5AyVE6Y8L5C7olITWh1oo2GwEz9044PeOauS3iR374aezyvIhE05o2TYssdpvo24QpZP+OA==" />
      
      <button type="submit" class="dropdown-item dropdown-signout" data-ga-click="Header, sign out, icon:logout" data-hydro-click="{&quot;event_type&quot;:&quot;global_header.user_menu_dropdown.click&quot;,&quot;payload&quot;:{&quot;request_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;target&quot;:&quot;SIGN_OUT&quot;,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}" data-hydro-click-hmac="a25d23e5023bcc088b105e01b8a35a1e184bf55a483ad07e08eda83da072cb33"  role="menuitem">
        Sign out
      </button>
      <input type="text" name="required_field_4844" hidden="hidden" class="form-control" /><input type="hidden" name="timestamp" value="1598880740742" class="form-control" /><input type="hidden" name="timestamp_secret" value="c3f0494881568caeaddb37a8ef1e0496ef5ef02991ff2d9e0960e0b87d11bdc1" class="form-control" />
</form>  </details-menu>
</details>

  </div>

</header>

          

    </div>

  <div id="start-of-content" class="show-on-focus"></div>




    <div id="js-flash-container">


  <template class="js-flash-template">
    <div class="flash flash-full  {{ className }}">
  <div class=" px-2" >
    <button class="flash-close js-flash-close" type="button" aria-label="Dismiss this message">
      <svg class="octicon octicon-x" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M3.72 3.72a.75.75 0 011.06 0L8 6.94l3.22-3.22a.75.75 0 111.06 1.06L9.06 8l3.22 3.22a.75.75 0 11-1.06 1.06L8 9.06l-3.22 3.22a.75.75 0 01-1.06-1.06L6.94 8 3.72 4.78a.75.75 0 010-1.06z"></path></svg>
    </button>
    
      <div>{{ message }}</div>

  </div>
</div>
  </template>
</div>


  

  <include-fragment class="js-notification-shelf-include-fragment" data-base-src="https://github.com/notifications/beta/shelf"></include-fragment>



  <div
    class="application-main "
    data-commit-hovercards-enabled
    data-discussion-hovercards-enabled
    data-issue-and-pr-hovercards-enabled
  >
        <div itemscope itemtype="http://schema.org/SoftwareSourceCode" class="">
    <main  >
      

  

      <div class="border-bottom shelf intro-shelf js-notice mb-0 pb-4">
  <div class="width-full container">
    <div class="width-full mx-auto shelf-content">
      <h2 class="shelf-title">Learn Git and GitHub without any code!</h2>
      <p class="shelf-lead">
          Using the Hello World guide, you’ll start a branch, write comments, and open a pull request.
      </p>
      <a class="btn btn-primary shelf-cta" target="_blank" data-hydro-click="{&quot;event_type&quot;:&quot;repository.click&quot;,&quot;payload&quot;:{&quot;target&quot;:&quot;READ_GUIDE&quot;,&quot;repository_id&quot;:262293514,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}" data-hydro-click-hmac="77078520fc0439e1c7d2f8fd6ca3bd5fa1be008b11ce64e0f1cc2f5b6f7fd1b1" href="https://guides.github.com/activities/hello-world/">Read the guide</a>
    </div>
    <!-- '"` --><!-- </textarea></xmp> --></option></form><form class="shelf-dismiss js-notice-dismiss" action="/dashboard/dismiss_bootcamp" accept-charset="UTF-8" method="post"><input type="hidden" name="_method" value="delete" /><input type="hidden" name="authenticity_token" value="C4UumqmKQUuvJdE1RUz+QMoM4W6L1lDIzgpLTrm0vM3br96XssD83g1fZHaYKEERLa4E/9qjL91H5j7r4d6hZg==" />
      <button name="button" type="submit" class="mr-1 close-button tooltipped tooltipped-w" aria-label="Hide this notice forever" data-hydro-click="{&quot;event_type&quot;:&quot;repository.click&quot;,&quot;payload&quot;:{&quot;target&quot;:&quot;DISMISS_BANNER&quot;,&quot;repository_id&quot;:262293514,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}" data-hydro-click-hmac="c0cfea4de95a2316c21860b8a70d88421b719dfa67f3359e87edf7178c8a0f02">
        <svg aria-label="Hide this notice forever" class="octicon octicon-x v-align-text-top" viewBox="0 0 16 16" version="1.1" width="16" height="16" role="img"><path fill-rule="evenodd" d="M3.72 3.72a.75.75 0 011.06 0L8 6.94l3.22-3.22a.75.75 0 111.06 1.06L9.06 8l3.22 3.22a.75.75 0 11-1.06 1.06L8 9.06l-3.22 3.22a.75.75 0 01-1.06-1.06L6.94 8 3.72 4.78a.75.75 0 010-1.06z"></path></svg>
</button></form>  </div>
</div>


  










  <div class="bg-gray-light pt-3 hide-full-screen mb-5">

    <div class="d-flex mb-3 px-3 px-md-4 px-lg-5">

      <div class="flex-auto min-width-0 width-fit mr-3">
        <h1 class=" d-flex flex-wrap flex-items-center break-word f3 text-normal">
    <svg class="octicon octicon-lock text-gray" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M4 4v2h-.25A1.75 1.75 0 002 7.75v5.5c0 .966.784 1.75 1.75 1.75h8.5A1.75 1.75 0 0014 13.25v-5.5A1.75 1.75 0 0012.25 6H12V4a4 4 0 10-8 0zm6.5 2V4a2.5 2.5 0 00-5 0v2h5zM12 7.5h.25a.25.25 0 01.25.25v5.5a.25.25 0 01-.25.25h-8.5a.25.25 0 01-.25-.25v-5.5a.25.25 0 01.25-.25H12z"></path></svg>
  <span class="author ml-2 flex-self-stretch" itemprop="author">
    <a class="url fn" rel="author" data-hovercard-type="organization" data-hovercard-url="/orgs/gabors-data-analysis/hovercard" href="/gabors-data-analysis">gabors-data-analysis</a>
  </span>
  <span class="mx-1 flex-self-stretch">/</span>
  <strong itemprop="name" class="mr-2 flex-self-stretch">
    <a data-pjax="#js-repo-pjax-container" href="/gabors-data-analysis/da_case_studies">da_case_studies</a>
  </strong>
  <span class="Label Label--outline v-align-middle ">Private</span>
</h1>


      </div>

      <ul class="pagehead-actions flex-shrink-0 d-none d-md-inline" style="padding: 2px 0;">

  <li>
        <form data-remote="true" class="d-flex js-social-form js-social-container" action="/notifications/subscribe" accept-charset="UTF-8" method="post"><input type="hidden" name="authenticity_token" value="cVTkYIwXH4hkDw2/cQwGb10SCF1OjynpchV8zWADzhJO23dMx/Hys15OAgMih25l3ZJnaKs4rHVMQ7gtefXaVw==" />      <input type="hidden" name="repository_id" value="262293514">

      <details class="details-reset details-overlay select-menu hx_rsm">
        <summary class="btn btn-sm btn-with-count" data-hydro-click="{&quot;event_type&quot;:&quot;repository.click&quot;,&quot;payload&quot;:{&quot;target&quot;:&quot;WATCH_BUTTON&quot;,&quot;repository_id&quot;:262293514,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}" data-hydro-click-hmac="90a75d788aca7e48d3c1219bcd7b547976dd7a0c5a30c47c9e3314ba96fb4ba0" data-ga-click="Repository, click Watch settings, action:blob#show">          <span data-menu-button>
              <svg height="16" class="octicon octicon-eye" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M1.679 7.932c.412-.621 1.242-1.75 2.366-2.717C5.175 4.242 6.527 3.5 8 3.5c1.473 0 2.824.742 3.955 1.715 1.124.967 1.954 2.096 2.366 2.717a.119.119 0 010 .136c-.412.621-1.242 1.75-2.366 2.717C10.825 11.758 9.473 12.5 8 12.5c-1.473 0-2.824-.742-3.955-1.715C2.92 9.818 2.09 8.69 1.679 8.068a.119.119 0 010-.136zM8 2c-1.981 0-3.67.992-4.933 2.078C1.797 5.169.88 6.423.43 7.1a1.619 1.619 0 000 1.798c.45.678 1.367 1.932 2.637 3.024C4.329 13.008 6.019 14 8 14c1.981 0 3.67-.992 4.933-2.078 1.27-1.091 2.187-2.345 2.637-3.023a1.619 1.619 0 000-1.798c-.45-.678-1.367-1.932-2.637-3.023C11.671 2.992 9.981 2 8 2zm0 8a2 2 0 100-4 2 2 0 000 4z"></path></svg>
              Watch
          </span>
          <span class="dropdown-caret"></span>
</summary>        <details-menu
          class="select-menu-modal position-absolute mt-5"
          style="z-index: 99;">
          <div class="select-menu-header">
            <span class="select-menu-title">Notifications</span>
          </div>
          <div class="select-menu-list">
            <button type="submit" name="do" value="included" class="select-menu-item width-full" aria-checked="true" role="menuitemradio">
              <svg class="octicon octicon-check select-menu-item-icon" height="16" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M13.78 4.22a.75.75 0 010 1.06l-7.25 7.25a.75.75 0 01-1.06 0L2.22 9.28a.75.75 0 011.06-1.06L6 10.94l6.72-6.72a.75.75 0 011.06 0z"></path></svg>
              <div class="select-menu-item-text">
                <span class="select-menu-item-heading">Not watching</span>
                <span class="description">Be notified only when participating or @mentioned.</span>
                <span class="hidden-select-button-text" data-menu-button-contents>
                  <svg height="16" class="octicon octicon-eye" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M1.679 7.932c.412-.621 1.242-1.75 2.366-2.717C5.175 4.242 6.527 3.5 8 3.5c1.473 0 2.824.742 3.955 1.715 1.124.967 1.954 2.096 2.366 2.717a.119.119 0 010 .136c-.412.621-1.242 1.75-2.366 2.717C10.825 11.758 9.473 12.5 8 12.5c-1.473 0-2.824-.742-3.955-1.715C2.92 9.818 2.09 8.69 1.679 8.068a.119.119 0 010-.136zM8 2c-1.981 0-3.67.992-4.933 2.078C1.797 5.169.88 6.423.43 7.1a1.619 1.619 0 000 1.798c.45.678 1.367 1.932 2.637 3.024C4.329 13.008 6.019 14 8 14c1.981 0 3.67-.992 4.933-2.078 1.27-1.091 2.187-2.345 2.637-3.023a1.619 1.619 0 000-1.798c-.45-.678-1.367-1.932-2.637-3.023C11.671 2.992 9.981 2 8 2zm0 8a2 2 0 100-4 2 2 0 000 4z"></path></svg>
                  Watch
                </span>
              </div>
            </button>

            <button type="submit" name="do" value="release_only" class="select-menu-item width-full" aria-checked="false" role="menuitemradio">
              <svg class="octicon octicon-check select-menu-item-icon" height="16" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M13.78 4.22a.75.75 0 010 1.06l-7.25 7.25a.75.75 0 01-1.06 0L2.22 9.28a.75.75 0 011.06-1.06L6 10.94l6.72-6.72a.75.75 0 011.06 0z"></path></svg>
              <div class="select-menu-item-text">
                <span class="select-menu-item-heading">Releases only</span>
                <span class="description">Be notified of new releases, and when participating or @mentioned.</span>
                <span class="hidden-select-button-text" data-menu-button-contents>
                  <svg height="16" class="octicon octicon-eye" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M1.679 7.932c.412-.621 1.242-1.75 2.366-2.717C5.175 4.242 6.527 3.5 8 3.5c1.473 0 2.824.742 3.955 1.715 1.124.967 1.954 2.096 2.366 2.717a.119.119 0 010 .136c-.412.621-1.242 1.75-2.366 2.717C10.825 11.758 9.473 12.5 8 12.5c-1.473 0-2.824-.742-3.955-1.715C2.92 9.818 2.09 8.69 1.679 8.068a.119.119 0 010-.136zM8 2c-1.981 0-3.67.992-4.933 2.078C1.797 5.169.88 6.423.43 7.1a1.619 1.619 0 000 1.798c.45.678 1.367 1.932 2.637 3.024C4.329 13.008 6.019 14 8 14c1.981 0 3.67-.992 4.933-2.078 1.27-1.091 2.187-2.345 2.637-3.023a1.619 1.619 0 000-1.798c-.45-.678-1.367-1.932-2.637-3.023C11.671 2.992 9.981 2 8 2zm0 8a2 2 0 100-4 2 2 0 000 4z"></path></svg>
                  Unwatch releases
                </span>
              </div>
            </button>

            <button type="submit" name="do" value="subscribed" class="select-menu-item width-full" aria-checked="false" role="menuitemradio">
              <svg class="octicon octicon-check select-menu-item-icon" height="16" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M13.78 4.22a.75.75 0 010 1.06l-7.25 7.25a.75.75 0 01-1.06 0L2.22 9.28a.75.75 0 011.06-1.06L6 10.94l6.72-6.72a.75.75 0 011.06 0z"></path></svg>
              <div class="select-menu-item-text">
                <span class="select-menu-item-heading">Watching</span>
                <span class="description">Be notified of all conversations.</span>
                <span class="hidden-select-button-text" data-menu-button-contents>
                  <svg class="octicon octicon-eye v-align-text-bottom" height="16" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M1.679 7.932c.412-.621 1.242-1.75 2.366-2.717C5.175 4.242 6.527 3.5 8 3.5c1.473 0 2.824.742 3.955 1.715 1.124.967 1.954 2.096 2.366 2.717a.119.119 0 010 .136c-.412.621-1.242 1.75-2.366 2.717C10.825 11.758 9.473 12.5 8 12.5c-1.473 0-2.824-.742-3.955-1.715C2.92 9.818 2.09 8.69 1.679 8.068a.119.119 0 010-.136zM8 2c-1.981 0-3.67.992-4.933 2.078C1.797 5.169.88 6.423.43 7.1a1.619 1.619 0 000 1.798c.45.678 1.367 1.932 2.637 3.024C4.329 13.008 6.019 14 8 14c1.981 0 3.67-.992 4.933-2.078 1.27-1.091 2.187-2.345 2.637-3.023a1.619 1.619 0 000-1.798c-.45-.678-1.367-1.932-2.637-3.023C11.671 2.992 9.981 2 8 2zm0 8a2 2 0 100-4 2 2 0 000 4z"></path></svg>
                  Unwatch
                </span>
              </div>
            </button>

            <button type="submit" name="do" value="ignore" class="select-menu-item width-full" aria-checked="false" role="menuitemradio">
              <svg class="octicon octicon-check select-menu-item-icon" height="16" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M13.78 4.22a.75.75 0 010 1.06l-7.25 7.25a.75.75 0 01-1.06 0L2.22 9.28a.75.75 0 011.06-1.06L6 10.94l6.72-6.72a.75.75 0 011.06 0z"></path></svg>
              <div class="select-menu-item-text">
                <span class="select-menu-item-heading">Ignoring</span>
                <span class="description">Never be notified.</span>
                <span class="hidden-select-button-text" data-menu-button-contents>
                  <svg height="16" class="octicon octicon-bell-slash" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M8 1.5c-.997 0-1.895.416-2.534 1.086A.75.75 0 014.38 1.55 5 5 0 0113 5v2.373a.75.75 0 01-1.5 0V5A3.5 3.5 0 008 1.5zM4.182 4.31L1.19 2.143a.75.75 0 10-.88 1.214L3 5.305v2.642a.25.25 0 01-.042.139L1.255 10.64A1.518 1.518 0 002.518 13h11.108l1.184.857a.75.75 0 10.88-1.214l-1.375-.996a1.196 1.196 0 00-.013-.01L4.198 4.321a.733.733 0 00-.016-.011zm7.373 7.19L4.5 6.391v1.556c0 .346-.102.683-.294.97l-1.703 2.556a.018.018 0 00-.003.01.015.015 0 00.005.012.017.017 0 00.006.004l.007.001h9.037zM8 16a2 2 0 001.985-1.75c.017-.137-.097-.25-.235-.25h-3.5c-.138 0-.252.113-.235.25A2 2 0 008 16z"></path></svg>
                  Stop ignoring
                </span>
              </div>
            </button>
          </div>
        </details-menu>
      </details>
        <a class="social-count js-social-count"
          href="/gabors-data-analysis/da_case_studies/watchers"
          aria-label="1 user is watching this repository">
          1
        </a>
</form>
  </li>

  <li>
      <div class="js-toggler-container js-social-container starring-container ">
    <form class="starred js-social-form" action="/gabors-data-analysis/da_case_studies/unstar" accept-charset="UTF-8" method="post"><input type="hidden" name="authenticity_token" value="GeUV/+LY/FbLSHvl9Vp5sOblecIjTz749DKA7rhwj1FwOxeGchd1w8B9wUNiTUQoDXPj2PZJf5elJqG7aNlb5Q==" />
      <input type="hidden" name="context" value="repository"></input>
      <button type="submit" class="btn btn-sm btn-with-count  js-toggler-target" aria-label="Unstar this repository" title="Unstar gabors-data-analysis/da_case_studies" data-hydro-click="{&quot;event_type&quot;:&quot;repository.click&quot;,&quot;payload&quot;:{&quot;target&quot;:&quot;UNSTAR_BUTTON&quot;,&quot;repository_id&quot;:262293514,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}" data-hydro-click-hmac="4e2b0a9a92eda46ff1d88fc42658b226cb2c215a5c0e38f7fe1d7aa2fcb8be99" data-ga-click="Repository, click unstar button, action:blob#show; text:Unstar">        <svg height="16" class="octicon octicon-star-fill" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M8 .25a.75.75 0 01.673.418l1.882 3.815 4.21.612a.75.75 0 01.416 1.279l-3.046 2.97.719 4.192a.75.75 0 01-1.088.791L8 12.347l-3.766 1.98a.75.75 0 01-1.088-.79l.72-4.194L.818 6.374a.75.75 0 01.416-1.28l4.21-.611L7.327.668A.75.75 0 018 .25z"></path></svg>
        Unstar
</button>        <a class="social-count js-social-count" href="/gabors-data-analysis/da_case_studies/stargazers"
           aria-label="0 users starred this repository">
           0
        </a>
</form>
    <form class="unstarred js-social-form" action="/gabors-data-analysis/da_case_studies/star" accept-charset="UTF-8" method="post"><input type="hidden" name="authenticity_token" value="SCB++zIJ3xg94aonfNiGvxqRgDQevlhH/+lE0eNExRgsXYKc1tbBtiHDfyIq4i8Q3gFRvbckQEvvRBsFQMmWiQ==" />
      <input type="hidden" name="context" value="repository"></input>
      <button type="submit" class="btn btn-sm btn-with-count  js-toggler-target" aria-label="Unstar this repository" title="Star gabors-data-analysis/da_case_studies" data-hydro-click="{&quot;event_type&quot;:&quot;repository.click&quot;,&quot;payload&quot;:{&quot;target&quot;:&quot;STAR_BUTTON&quot;,&quot;repository_id&quot;:262293514,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}" data-hydro-click-hmac="54c8589b417902543b779cab098d3569c061753b5ae0ee0031fa956381a5c29e" data-ga-click="Repository, click star button, action:blob#show; text:Star">        <svg height="16" class="octicon octicon-star" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M8 .25a.75.75 0 01.673.418l1.882 3.815 4.21.612a.75.75 0 01.416 1.279l-3.046 2.97.719 4.192a.75.75 0 01-1.088.791L8 12.347l-3.766 1.98a.75.75 0 01-1.088-.79l.72-4.194L.818 6.374a.75.75 0 01.416-1.28l4.21-.611L7.327.668A.75.75 0 018 .25zm0 2.445L6.615 5.5a.75.75 0 01-.564.41l-3.097.45 2.24 2.184a.75.75 0 01.216.664l-.528 3.084 2.769-1.456a.75.75 0 01.698 0l2.77 1.456-.53-3.084a.75.75 0 01.216-.664l2.24-2.183-3.096-.45a.75.75 0 01-.564-.41L8 2.694v.001z"></path></svg>
        Star
</button>        <a class="social-count js-social-count" href="/gabors-data-analysis/da_case_studies/stargazers"
           aria-label="0 users starred this repository">
          0
        </a>
</form>  </div>

  </li>

  <li>
          <div class="float-left">
            <details class="details-reset details-overlay details-overlay-dark " >
                    <summary
                  class="btn btn-sm btn-with-count"
                  title="Fork your own copy of gabors-data-analysis/da_case_studies to your account"
                  data-hydro-click="{&quot;event_type&quot;:&quot;repository.click&quot;,&quot;payload&quot;:{&quot;target&quot;:&quot;FORK_BUTTON&quot;,&quot;repository_id&quot;:262293514,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}" data-hydro-click-hmac="f4014b757589582508db2245be8148545044f928f820e3245d1ca2fe776541ef" data-ga-click="Repository, show fork modal, action:blob#show; text:Fork">
                  <svg class="octicon octicon-repo-forked" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M5 3.25a.75.75 0 11-1.5 0 .75.75 0 011.5 0zm0 2.122a2.25 2.25 0 10-1.5 0v.878A2.25 2.25 0 005.75 8.5h1.5v2.128a2.251 2.251 0 101.5 0V8.5h1.5a2.25 2.25 0 002.25-2.25v-.878a2.25 2.25 0 10-1.5 0v.878a.75.75 0 01-.75.75h-4.5A.75.75 0 015 6.25v-.878zm3.75 7.378a.75.75 0 11-1.5 0 .75.75 0 011.5 0zm3-8.75a.75.75 0 100-1.5.75.75 0 000 1.5z"></path></svg>
                  Fork
                </summary>

  <details-dialog
    class="Box d-flex flex-column anim-fade-in fast Box--overlay"
      aria-label="Fork da_case_studies"
      src="/gabors-data-analysis/da_case_studies/fork?fragment=1"
      preload
    >
    <div class="Box-header">
      <button class="Box-btn-octicon btn-octicon float-right" type="button" aria-label="Close dialog" data-close-dialog>
        <svg height="16" class="octicon octicon-x" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M3.72 3.72a.75.75 0 011.06 0L8 6.94l3.22-3.22a.75.75 0 111.06 1.06L9.06 8l3.22 3.22a.75.75 0 11-1.06 1.06L8 9.06l-3.22 3.22a.75.75 0 01-1.06-1.06L6.94 8 3.72 4.78a.75.75 0 010-1.06z"></path></svg>
      </button>
      <h1 class="Box-title">Fork da_case_studies</h1>
    </div>
      
              <div class="text-center overflow-auto">
                <include-fragment>
                  <div class="octocat-spinner my-5" aria-label="Loading..."></div>
                  <p class="f5 text-gray">If this dialog fails to load, you can visit <a href="/gabors-data-analysis/da_case_studies/fork">the fork page</a> directly.</p>
                </include-fragment>
              </div>

  </details-dialog>
</details>
          </div>

    <a href="/gabors-data-analysis/da_case_studies/network/members" class="social-count"
       aria-label="0 users forked this repository">
      0
    </a>
  </li>
</ul>

    </div>
    
<nav aria-label="Repository" data-pjax="#js-repo-pjax-container" class="js-repo-nav js-sidenav-container-pjax js-responsive-underlinenav overflow-hidden UnderlineNav px-3 px-md-4 px-lg-5 bg-gray-light">
  <ul class="UnderlineNav-body list-style-none ">
          <li class="d-flex">
        <a class="js-selected-navigation-item selected UnderlineNav-item hx_underlinenav-item no-wrap js-responsive-underlinenav-item" data-tab-item="code-tab" data-hotkey="g c" data-ga-click="Repository, Navigation click, Code tab" aria-current="page" data-selected-links="repo_source repo_downloads repo_commits repo_releases repo_tags repo_branches repo_packages repo_deployments /gabors-data-analysis/da_case_studies" href="/gabors-data-analysis/da_case_studies">
              <svg classes="UnderlineNav-octicon" display="none inline" height="16" class="octicon octicon-code UnderlineNav-octicon d-none d-sm-inline" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M4.72 3.22a.75.75 0 011.06 1.06L2.06 8l3.72 3.72a.75.75 0 11-1.06 1.06L.47 8.53a.75.75 0 010-1.06l4.25-4.25zm6.56 0a.75.75 0 10-1.06 1.06L13.94 8l-3.72 3.72a.75.75 0 101.06 1.06l4.25-4.25a.75.75 0 000-1.06l-4.25-4.25z"></path></svg>
            <span data-content="Code">Code</span>
              <span title="Not available" class="Counter "></span>
</a>      </li>
      <li class="d-flex">
        <a class="js-selected-navigation-item UnderlineNav-item hx_underlinenav-item no-wrap js-responsive-underlinenav-item" data-tab-item="issues-tab" data-hotkey="g i" data-ga-click="Repository, Navigation click, Issues tab" data-selected-links="repo_issues repo_labels repo_milestones /gabors-data-analysis/da_case_studies/issues" href="/gabors-data-analysis/da_case_studies/issues">
              <svg classes="UnderlineNav-octicon" display="none inline" height="16" class="octicon octicon-issue-opened UnderlineNav-octicon d-none d-sm-inline" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M8 1.5a6.5 6.5 0 100 13 6.5 6.5 0 000-13zM0 8a8 8 0 1116 0A8 8 0 010 8zm9 3a1 1 0 11-2 0 1 1 0 012 0zm-.25-6.25a.75.75 0 00-1.5 0v3.5a.75.75 0 001.5 0v-3.5z"></path></svg>
            <span data-content="Issues">Issues</span>
              <span title="11" class="Counter ">11</span>
</a>      </li>
      <li class="d-flex">
        <a class="js-selected-navigation-item UnderlineNav-item hx_underlinenav-item no-wrap js-responsive-underlinenav-item" data-tab-item="pull-requests-tab" data-hotkey="g p" data-ga-click="Repository, Navigation click, Pull requests tab" data-selected-links="repo_pulls checks /gabors-data-analysis/da_case_studies/pulls" href="/gabors-data-analysis/da_case_studies/pulls">
              <svg classes="UnderlineNav-octicon" display="none inline" height="16" class="octicon octicon-git-pull-request UnderlineNav-octicon d-none d-sm-inline" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M7.177 3.073L9.573.677A.25.25 0 0110 .854v4.792a.25.25 0 01-.427.177L7.177 3.427a.25.25 0 010-.354zM3.75 2.5a.75.75 0 100 1.5.75.75 0 000-1.5zm-2.25.75a2.25 2.25 0 113 2.122v5.256a2.251 2.251 0 11-1.5 0V5.372A2.25 2.25 0 011.5 3.25zM11 2.5h-1V4h1a1 1 0 011 1v5.628a2.251 2.251 0 101.5 0V5A2.5 2.5 0 0011 2.5zm1 10.25a.75.75 0 111.5 0 .75.75 0 01-1.5 0zM3.75 12a.75.75 0 100 1.5.75.75 0 000-1.5z"></path></svg>
            <span data-content="Pull requests">Pull requests</span>
              <span title="0" hidden="hidden" class="Counter ">0</span>
</a>      </li>
      <li class="d-flex">
        <a class="js-selected-navigation-item UnderlineNav-item hx_underlinenav-item no-wrap js-responsive-underlinenav-item" data-tab-item="actions-tab" data-hotkey="g a" data-ga-click="Repository, Navigation click, Actions tab" data-selected-links="repo_actions /gabors-data-analysis/da_case_studies/actions" href="/gabors-data-analysis/da_case_studies/actions">
              <svg classes="UnderlineNav-octicon" display="none inline" height="16" class="octicon octicon-play UnderlineNav-octicon d-none d-sm-inline" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M1.5 8a6.5 6.5 0 1113 0 6.5 6.5 0 01-13 0zM8 0a8 8 0 100 16A8 8 0 008 0zM6.379 5.227A.25.25 0 006 5.442v5.117a.25.25 0 00.379.214l4.264-2.559a.25.25 0 000-.428L6.379 5.227z"></path></svg>
            <span data-content="Actions">Actions</span>
              <span title="Not available" class="Counter "></span>
</a>      </li>
      <li class="d-flex">
        <a class="js-selected-navigation-item UnderlineNav-item hx_underlinenav-item no-wrap js-responsive-underlinenav-item" data-tab-item="projects-tab" data-hotkey="g b" data-ga-click="Repository, Navigation click, Projects tab" data-selected-links="repo_projects new_repo_project repo_project /gabors-data-analysis/da_case_studies/projects" href="/gabors-data-analysis/da_case_studies/projects">
              <svg classes="UnderlineNav-octicon" display="none inline" height="16" class="octicon octicon-project UnderlineNav-octicon d-none d-sm-inline" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M1.75 0A1.75 1.75 0 000 1.75v12.5C0 15.216.784 16 1.75 16h12.5A1.75 1.75 0 0016 14.25V1.75A1.75 1.75 0 0014.25 0H1.75zM1.5 1.75a.25.25 0 01.25-.25h12.5a.25.25 0 01.25.25v12.5a.25.25 0 01-.25.25H1.75a.25.25 0 01-.25-.25V1.75zM11.75 3a.75.75 0 00-.75.75v7.5a.75.75 0 001.5 0v-7.5a.75.75 0 00-.75-.75zm-8.25.75a.75.75 0 011.5 0v5.5a.75.75 0 01-1.5 0v-5.5zM8 3a.75.75 0 00-.75.75v3.5a.75.75 0 001.5 0v-3.5A.75.75 0 008 3z"></path></svg>
            <span data-content="Projects">Projects</span>
              <span title="0" hidden="hidden" class="Counter ">0</span>
</a>      </li>
      <li class="d-flex">
        <a class="js-selected-navigation-item UnderlineNav-item hx_underlinenav-item no-wrap js-responsive-underlinenav-item" data-tab-item="security-tab" data-hotkey="g s" data-ga-click="Repository, Navigation click, Security tab" data-selected-links="security overview alerts policy token_scanning code_scanning /gabors-data-analysis/da_case_studies/security" href="/gabors-data-analysis/da_case_studies/security">
              <svg classes="UnderlineNav-octicon" display="none inline" height="16" class="octicon octicon-shield UnderlineNav-octicon d-none d-sm-inline" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M7.467.133a1.75 1.75 0 011.066 0l5.25 1.68A1.75 1.75 0 0115 3.48V7c0 1.566-.32 3.182-1.303 4.682-.983 1.498-2.585 2.813-5.032 3.855a1.7 1.7 0 01-1.33 0c-2.447-1.042-4.049-2.357-5.032-3.855C1.32 10.182 1 8.566 1 7V3.48a1.75 1.75 0 011.217-1.667l5.25-1.68zm.61 1.429a.25.25 0 00-.153 0l-5.25 1.68a.25.25 0 00-.174.238V7c0 1.358.275 2.666 1.057 3.86.784 1.194 2.121 2.34 4.366 3.297a.2.2 0 00.154 0c2.245-.956 3.582-2.104 4.366-3.298C13.225 9.666 13.5 8.36 13.5 7V3.48a.25.25 0 00-.174-.237l-5.25-1.68zM9 10.5a1 1 0 11-2 0 1 1 0 012 0zm-.25-5.75a.75.75 0 10-1.5 0v3a.75.75 0 001.5 0v-3z"></path></svg>
            <span data-content="Security">Security</span>
              <span data-url="/gabors-data-analysis/da_case_studies/security/overall-count" title="Not available" class="js-security-tab-count Counter "></span>
</a>      </li>
      <li class="d-flex">
        <a class="js-selected-navigation-item UnderlineNav-item hx_underlinenav-item no-wrap js-responsive-underlinenav-item" data-tab-item="insights-tab" data-ga-click="Repository, Navigation click, Insights tab" data-selected-links="repo_graphs repo_contributors dependency_graph dependabot_updates pulse people /gabors-data-analysis/da_case_studies/network/dependencies" href="/gabors-data-analysis/da_case_studies/network/dependencies">
              <svg classes="UnderlineNav-octicon" display="none inline" height="16" class="octicon octicon-graph UnderlineNav-octicon d-none d-sm-inline" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M1.5 1.75a.75.75 0 00-1.5 0v12.5c0 .414.336.75.75.75h14.5a.75.75 0 000-1.5H1.5V1.75zm14.28 2.53a.75.75 0 00-1.06-1.06L10 7.94 7.53 5.47a.75.75 0 00-1.06 0L3.22 8.72a.75.75 0 001.06 1.06L7 7.06l2.47 2.47a.75.75 0 001.06 0l5.25-5.25z"></path></svg>
            <span data-content="Insights">Insights</span>
              <span title="Not available" class="Counter "></span>
</a>      </li>

</ul>        <div class="position-absolute right-0 pr-3 pr-md-4 pr-lg-5 js-responsive-underlinenav-overflow" style="visibility:hidden;">
      <details class="details-overlay details-reset position-relative">
  <summary role="button">
              <div class="UnderlineNav-item mr-0 border-0">
            <svg class="octicon octicon-kebab-horizontal" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path d="M8 9a1.5 1.5 0 100-3 1.5 1.5 0 000 3zM1.5 9a1.5 1.5 0 100-3 1.5 1.5 0 000 3zm13 0a1.5 1.5 0 100-3 1.5 1.5 0 000 3z"></path></svg>
            <span class="sr-only">More</span>
          </div>

</summary>            <details-menu role="menu" class="dropdown-menu dropdown-menu-sw ">
  
            <ul>
                <li data-menu-item="code-tab" hidden>
                  <a role="menuitem" class="js-selected-navigation-item dropdown-item" data-selected-links=" /gabors-data-analysis/da_case_studies" href="/gabors-data-analysis/da_case_studies">
                    Code
</a>                </li>
                <li data-menu-item="issues-tab" hidden>
                  <a role="menuitem" class="js-selected-navigation-item dropdown-item" data-selected-links=" /gabors-data-analysis/da_case_studies/issues" href="/gabors-data-analysis/da_case_studies/issues">
                    Issues
</a>                </li>
                <li data-menu-item="pull-requests-tab" hidden>
                  <a role="menuitem" class="js-selected-navigation-item dropdown-item" data-selected-links=" /gabors-data-analysis/da_case_studies/pulls" href="/gabors-data-analysis/da_case_studies/pulls">
                    Pull requests
</a>                </li>
                <li data-menu-item="actions-tab" hidden>
                  <a role="menuitem" class="js-selected-navigation-item dropdown-item" data-selected-links=" /gabors-data-analysis/da_case_studies/actions" href="/gabors-data-analysis/da_case_studies/actions">
                    Actions
</a>                </li>
                <li data-menu-item="projects-tab" hidden>
                  <a role="menuitem" class="js-selected-navigation-item dropdown-item" data-selected-links=" /gabors-data-analysis/da_case_studies/projects" href="/gabors-data-analysis/da_case_studies/projects">
                    Projects
</a>                </li>
                <li data-menu-item="security-tab" hidden>
                  <a role="menuitem" class="js-selected-navigation-item dropdown-item" data-selected-links=" /gabors-data-analysis/da_case_studies/security" href="/gabors-data-analysis/da_case_studies/security">
                    Security
</a>                </li>
                <li data-menu-item="insights-tab" hidden>
                  <a role="menuitem" class="js-selected-navigation-item dropdown-item" data-selected-links=" /gabors-data-analysis/da_case_studies/network/dependencies" href="/gabors-data-analysis/da_case_studies/network/dependencies">
                    Insights
</a>                </li>
            </ul>

</details-menu>
</details>    </div>

</nav>
  </div>

<div class="container-xl clearfix new-discussion-timeline  px-3 px-md-4 px-lg-5">
  <div class="repository-content " >

    
    
  


    <a class="d-none js-permalink-shortcut" data-hotkey="y" href="/gabors-data-analysis/da_case_studies/blob/5cbdf3e320c2a27257601507105e42583ab03cb8/ch18-case-shiller-la/ch18-ts-pred-housprices.R">Permalink</a>

    <!-- blob contrib key: blob_contributors:v22:73c721928254df9efa0104bc5d9409bd -->
    

    <div class="d-flex flex-items-start flex-shrink-0 pb-3 flex-wrap flex-md-nowrap flex-justify-between flex-md-justify-start">
      
<div class="position-relative">
  <details class="details-reset details-overlay mr-0 mb-0 " id="branch-select-menu">
    <summary class="btn css-truncate"
            data-hotkey="w"
            title="Switch branches or tags">
      <svg text="gray" height="16" class="octicon octicon-git-branch text-gray" viewBox="0 0 16 16" version="1.1" width="16" aria-hidden="true"><path fill-rule="evenodd" d="M11.75 2.5a.75.75 0 100 1.5.75.75 0 000-1.5zm-2.25.75a2.25 2.25 0 113 2.122V6A2.5 2.5 0 0110 8.5H6a1 1 0 00-1 1v1.128a2.251 2.251 0 11-1.5 0V5.372a2.25 2.25 0 111.5 0v1.836A2.492 2.492 0 016 7h4a1 1 0 001-1v-.628A2.25 2.25 0 019.5 3.25zM4.25 12a.75.75 0 100 1.5.75.75 0 000-1.5zM3.5 3.25a.75.75 0 111.5 0 .75.75 0 01-1.5 0z"></path></svg>
      <span class="css-truncate-target" data-menu-button>master</span>
      <span class="dropdown-caret"></span>
    </summary>

    <details-menu class="SelectMenu SelectMenu--hasFilter" src="/gabors-data-analysis/da_case_studies/refs/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R?source_action=show&amp;source_controller=blob" preload>
      <div class="SelectMenu-modal">
        <include-fragment class="SelectMenu-loading" aria-label="Menu is loading">
          <svg class="octicon octicon-octoface anim-pulse" height="32" viewBox="0 0 24 24" version="1.1" width="32" aria-hidden="true"><path d="M7.75 11c-.69 0-1.25.56-1.25 1.25v1.5a1.25 1.25 0 102.5 0v-1.5C9 11.56 8.44 11 7.75 11zm1.27 4.5a.469.469 0 01.48-.5h5a.47.47 0 01.48.5c-.116 1.316-.759 2.5-2.98 2.5s-2.864-1.184-2.98-2.5zm7.23-4.5c-.69 0-1.25.56-1.25 1.25v1.5a1.25 1.25 0 102.5 0v-1.5c0-.69-.56-1.25-1.25-1.25z"></path><path fill-rule="evenodd" d="M21.255 3.82a1.725 1.725 0 00-2.141-1.195c-.557.16-1.406.44-2.264.866-.78.386-1.647.93-2.293 1.677A18.442 18.442 0 0012 5c-.93 0-1.784.059-2.569.17-.645-.74-1.505-1.28-2.28-1.664a13.876 13.876 0 00-2.265-.866 1.725 1.725 0 00-2.141 1.196 23.645 23.645 0 00-.69 3.292c-.125.97-.191 2.07-.066 3.112C1.254 11.882 1 13.734 1 15.527 1 19.915 3.13 23 12 23c8.87 0 11-3.053 11-7.473 0-1.794-.255-3.647-.99-5.29.127-1.046.06-2.15-.066-3.125a23.652 23.652 0 00-.689-3.292zM20.5 14c.5 3.5-1.5 6.5-8.5 6.5s-9-3-8.5-6.5c.583-4 3-6 8.5-6s7.928 2 8.5 6z"></path></svg>
        </include-fragment>
      </div>
    </details-menu>
  </details>

</div>

      <h2 id="blob-path" class="breadcrumb flex-auto min-width-0 text-normal mx-0 mx-md-3 width-full width-md-auto flex-order-1 flex-md-order-none mt-3 mt-md-0">
        <span class="js-repo-root text-bold"><span class="js-path-segment d-inline-block wb-break-all"><a data-pjax="true" href="/gabors-data-analysis/da_case_studies"><span>da_case_studies</span></a></span></span><span class="separator">/</span><span class="js-path-segment d-inline-block wb-break-all"><a data-pjax="true" href="/gabors-data-analysis/da_case_studies/tree/master/ch18-case-shiller-la"><span>ch18-case-shiller-la</span></a></span><span class="separator">/</span><strong class="final-path">ch18-ts-pred-housprices.R</strong>
      </h2>
      <a href="/gabors-data-analysis/da_case_studies/find/master"
            class="js-pjax-capture-input btn mr-2 d-none d-md-block"
            data-pjax
            data-hotkey="t">
        Go to file
      </a>

      <details id="blob-more-options-details" class="details-overlay details-reset position-relative">
  <summary role="button">
              <span class="btn">
            <svg aria-label="More options" height="16" class="octicon octicon-kebab-horizontal" viewBox="0 0 16 16" version="1.1" width="16" role="img"><path d="M8 9a1.5 1.5 0 100-3 1.5 1.5 0 000 3zM1.5 9a1.5 1.5 0 100-3 1.5 1.5 0 000 3zm13 0a1.5 1.5 0 100-3 1.5 1.5 0 000 3z"></path></svg>
          </span>

</summary>            <ul class="dropdown-menu dropdown-menu-sw">
            <li class="d-block d-md-none">
              <a class="dropdown-item d-flex flex-items-baseline" data-hydro-click="{&quot;event_type&quot;:&quot;repository.click&quot;,&quot;payload&quot;:{&quot;target&quot;:&quot;FIND_FILE_BUTTON&quot;,&quot;repository_id&quot;:262293514,&quot;originating_url&quot;:&quot;https://github.com/gabors-data-analysis/da_case_studies/blob/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R&quot;,&quot;user_id&quot;:54452084}}" data-hydro-click-hmac="fbde90f66e48c8310100b8cf4dc99e42b1e3ae7ae106fee88e7e250324ca2120" data-ga-click="Repository, find file, location:repo overview" data-hotkey="t" data-pjax="true" href="/gabors-data-analysis/da_case_studies/find/master">
                <span class="flex-auto">Go to file</span>
                <span class="text-small text-gray" aria-hidden="true">T</span>
</a>            </li>
            <li data-toggle-for="blob-more-options-details">
              <button type="button" data-toggle-for="jumpto-line-details-dialog" class="btn-link dropdown-item">
                <span class="d-flex flex-items-baseline">
                  <span class="flex-auto">Go to line</span>
                  <span class="text-small text-gray" aria-hidden="true">L</span>
                </span>
              </button>
            </li>
            <li class="dropdown-divider" role="none"></li>
            <li>
              <clipboard-copy value="ch18-case-shiller-la/ch18-ts-pred-housprices.R" class="dropdown-item cursor-pointer" data-toggle-for="blob-more-options-details">
                Copy path
              </clipboard-copy>
            </li>
          </ul>

</details>    </div>



    <div class="Box d-flex flex-column flex-shrink-0 mb-3">
      <include-fragment src="/gabors-data-analysis/da_case_studies/contributors/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R" class="commit-loader">
        <div class="Box-header Box-header--blue d-flex flex-items-center">
          <div class="Skeleton avatar avatar-user flex-shrink-0 ml-n1 mr-n1 mt-n1 mb-n1" style="width:24px;height:24px;"></div>
          <div class="Skeleton Skeleton--text col-5 ml-2">&nbsp;</div>
        </div>

        <div class="Box-body d-flex flex-items-center" >
          <div class="Skeleton Skeleton--text col-1">&nbsp;</div>
          <span class="text-red h6 loader-error">Cannot retrieve contributors at this time</span>
        </div>
</include-fragment>    </div>






    <div class="Box mt-3 position-relative
      ">
      
<div class="Box-header py-2 d-flex flex-column flex-shrink-0 flex-md-row flex-md-items-center">
  <div class="text-mono f6 flex-auto pr-3 flex-order-2 flex-md-order-1 mt-2 mt-md-0">

      691 lines (549 sloc)
      <span class="file-info-divider"></span>
    20 KB
  </div>

  <div class="d-flex py-1 py-md-0 flex-auto flex-order-1 flex-md-order-2 flex-sm-grow-0 flex-justify-between">

    <div class="BtnGroup">
      <a href="/gabors-data-analysis/da_case_studies/raw/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R" id="raw-url" role="button" class="btn btn-sm BtnGroup-item ">Raw</a>
        <a href="/gabors-data-analysis/da_case_studies/blame/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R" data-hotkey="b" role="button" class="btn js-update-url-with-hash btn-sm BtnGroup-item ">Blame</a>
    </div>

    <div>
          <a class="btn-octicon tooltipped tooltipped-nw js-remove-unless-platform"
             data-platforms="windows,mac"
             href="x-github-client://openRepo/https://github.com/gabors-data-analysis/da_case_studies?branch=master&amp;filepath=ch18-case-shiller-la%2Fch18-ts-pred-housprices.R"
             aria-label="Open this file in GitHub Desktop"
             data-ga-click="Repository, open with desktop">
              <svg class="octicon octicon-device-desktop" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M1.75 2.5h12.5a.25.25 0 01.25.25v7.5a.25.25 0 01-.25.25H1.75a.25.25 0 01-.25-.25v-7.5a.25.25 0 01.25-.25zM14.25 1H1.75A1.75 1.75 0 000 2.75v7.5C0 11.216.784 12 1.75 12h3.727c-.1 1.041-.52 1.872-1.292 2.757A.75.75 0 004.75 16h6.5a.75.75 0 00.565-1.243c-.772-.885-1.193-1.716-1.292-2.757h3.727A1.75 1.75 0 0016 10.25v-7.5A1.75 1.75 0 0014.25 1zM9.018 12H6.982a5.72 5.72 0 01-.765 2.5h3.566a5.72 5.72 0 01-.765-2.5z"></path></svg>
          </a>

          <!-- '"` --><!-- </textarea></xmp> --></option></form><form class="inline-form js-update-url-with-hash" action="/gabors-data-analysis/da_case_studies/edit/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R" accept-charset="UTF-8" method="post"><input type="hidden" name="authenticity_token" value="5rogrerd8mc8T6YNlaY6UjAMliLKxKyCb/2vdegq6vrj29UsmllAb+8ZbW4l1GNL8O5obx1qA+U9P/qjLYn13g==" />
            <button class="btn-octicon tooltipped tooltipped-nw" type="submit"
              aria-label="Edit this file" data-hotkey="e" data-disable-with>
              <svg class="octicon octicon-pencil" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M11.013 1.427a1.75 1.75 0 012.474 0l1.086 1.086a1.75 1.75 0 010 2.474l-8.61 8.61c-.21.21-.47.364-.756.445l-3.251.93a.75.75 0 01-.927-.928l.929-3.25a1.75 1.75 0 01.445-.758l8.61-8.61zm1.414 1.06a.25.25 0 00-.354 0L10.811 3.75l1.439 1.44 1.263-1.263a.25.25 0 000-.354l-1.086-1.086zM11.189 6.25L9.75 4.81l-6.286 6.287a.25.25 0 00-.064.108l-.558 1.953 1.953-.558a.249.249 0 00.108-.064l6.286-6.286z"></path></svg>
            </button>
</form>
          <!-- '"` --><!-- </textarea></xmp> --></option></form><form class="inline-form" action="/gabors-data-analysis/da_case_studies/delete/master/ch18-case-shiller-la/ch18-ts-pred-housprices.R" accept-charset="UTF-8" method="post"><input type="hidden" name="authenticity_token" value="IZ3og/Ra/0LNpv1Jd3FPVOHCQXsMdiSUU1U3kfN16UoO1Rh2SERP1PiY2/Q5a4DfGek9ypzzPtjFcCMZLNbIxg==" />
            <button class="btn-octicon btn-octicon-danger tooltipped tooltipped-nw" type="submit"
              aria-label="Delete this file" data-disable-with>
              <svg class="octicon octicon-trashcan" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M6.5 1.75a.25.25 0 01.25-.25h2.5a.25.25 0 01.25.25V3h-3V1.75zm4.5 0V3h2.25a.75.75 0 010 1.5H2.75a.75.75 0 010-1.5H5V1.75C5 .784 5.784 0 6.75 0h2.5C10.216 0 11 .784 11 1.75zM4.496 6.675a.75.75 0 10-1.492.15l.66 6.6A1.75 1.75 0 005.405 15h5.19c.9 0 1.652-.681 1.741-1.576l.66-6.6a.75.75 0 00-1.492-.149l-.66 6.6a.25.25 0 01-.249.225h-5.19a.25.25 0 01-.249-.225l-.66-6.6z"></path></svg>
            </button>
</form>    </div>
  </div>
</div>



      

  <div itemprop="text" class="Box-body p-0 blob-wrapper data type-r  gist-border-0">
      
<table class="highlight tab-size js-file-line-container" data-tab-size="8" data-paste-markdown-skip>
      <tr>
        <td id="L1" class="blob-num js-line-number" data-line-number="1"></td>
        <td id="LC1" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>###############################################################################################</span></td>
      </tr>
      <tr>
        <td id="L2" class="blob-num js-line-number" data-line-number="2"></td>
        <td id="LC2" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Prepared for the textbook:</span></td>
      </tr>
      <tr>
        <td id="L3" class="blob-num js-line-number" data-line-number="3"></td>
        <td id="LC3" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Data Analysis for Business, Economics, and Policy</span></td>
      </tr>
      <tr>
        <td id="L4" class="blob-num js-line-number" data-line-number="4"></td>
        <td id="LC4" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> by Gabor BEKES and  Gabor KEZDI </span></td>
      </tr>
      <tr>
        <td id="L5" class="blob-num js-line-number" data-line-number="5"></td>
        <td id="LC5" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Cambridge University Press 2021</span></td>
      </tr>
      <tr>
        <td id="L6" class="blob-num js-line-number" data-line-number="6"></td>
        <td id="LC6" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> </span></td>
      </tr>
      <tr>
        <td id="L7" class="blob-num js-line-number" data-line-number="7"></td>
        <td id="LC7" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> License: Free to share, modify and use for educational purposes. Not to be used for business purposes.</span></td>
      </tr>
      <tr>
        <td id="L8" class="blob-num js-line-number" data-line-number="8"></td>
        <td id="LC8" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span></span></td>
      </tr>
      <tr>
        <td id="L9" class="blob-num js-line-number" data-line-number="9"></td>
        <td id="LC9" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>##############################################################################################x</span></td>
      </tr>
      <tr>
        <td id="L10" class="blob-num js-line-number" data-line-number="10"></td>
        <td id="LC10" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L11" class="blob-num js-line-number" data-line-number="11"></td>
        <td id="LC11" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> CHAPTER 18</span></td>
      </tr>
      <tr>
        <td id="L12" class="blob-num js-line-number" data-line-number="12"></td>
        <td id="LC12" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> CH18B Forecasting a house price index</span></td>
      </tr>
      <tr>
        <td id="L13" class="blob-num js-line-number" data-line-number="13"></td>
        <td id="LC13" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> case-shiller-la dataset</span></td>
      </tr>
      <tr>
        <td id="L14" class="blob-num js-line-number" data-line-number="14"></td>
        <td id="LC14" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> version 0.9 2020-08-28</span></td>
      </tr>
      <tr>
        <td id="L15" class="blob-num js-line-number" data-line-number="15"></td>
        <td id="LC15" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L16" class="blob-num js-line-number" data-line-number="16"></td>
        <td id="LC16" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L17" class="blob-num js-line-number" data-line-number="17"></td>
        <td id="LC17" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> ------------------------------------------------------------------------------------------------------</span></td>
      </tr>
      <tr>
        <td id="L18" class="blob-num js-line-number" data-line-number="18"></td>
        <td id="LC18" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>### SET UP</span></td>
      </tr>
      <tr>
        <td id="L19" class="blob-num js-line-number" data-line-number="19"></td>
        <td id="LC19" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> It is advised to start a new session for every case study</span></td>
      </tr>
      <tr>
        <td id="L20" class="blob-num js-line-number" data-line-number="20"></td>
        <td id="LC20" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> CLEAR MEMORY</span></td>
      </tr>
      <tr>
        <td id="L21" class="blob-num js-line-number" data-line-number="21"></td>
        <td id="LC21" class="blob-code blob-code-inner js-file-line">rm(<span class="pl-v">list</span><span class="pl-k">=</span>ls())</td>
      </tr>
      <tr>
        <td id="L22" class="blob-num js-line-number" data-line-number="22"></td>
        <td id="LC22" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L23" class="blob-num js-line-number" data-line-number="23"></td>
        <td id="LC23" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Import libraries</span></td>
      </tr>
      <tr>
        <td id="L24" class="blob-num js-line-number" data-line-number="24"></td>
        <td id="LC24" class="blob-code blob-code-inner js-file-line">library(<span class="pl-smi">tidyverse</span>)</td>
      </tr>
      <tr>
        <td id="L25" class="blob-num js-line-number" data-line-number="25"></td>
        <td id="LC25" class="blob-code blob-code-inner js-file-line">library(<span class="pl-smi">fpp3</span>)</td>
      </tr>
      <tr>
        <td id="L26" class="blob-num js-line-number" data-line-number="26"></td>
        <td id="LC26" class="blob-code blob-code-inner js-file-line">library(<span class="pl-smi">cowplot</span>)</td>
      </tr>
      <tr>
        <td id="L27" class="blob-num js-line-number" data-line-number="27"></td>
        <td id="LC27" class="blob-code blob-code-inner js-file-line">library(<span class="pl-smi">lubridate</span>)</td>
      </tr>
      <tr>
        <td id="L28" class="blob-num js-line-number" data-line-number="28"></td>
        <td id="LC28" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L29" class="blob-num js-line-number" data-line-number="29"></td>
        <td id="LC29" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> set working directory</span></td>
      </tr>
      <tr>
        <td id="L30" class="blob-num js-line-number" data-line-number="30"></td>
        <td id="LC30" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> option A: open material as project</span></td>
      </tr>
      <tr>
        <td id="L31" class="blob-num js-line-number" data-line-number="31"></td>
        <td id="LC31" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> option B: set working directory for da_case_studies</span></td>
      </tr>
      <tr>
        <td id="L32" class="blob-num js-line-number" data-line-number="32"></td>
        <td id="LC32" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>           example: setwd(&quot;C:/Users/bekes.gabor/Documents/github/da_case_studies/&quot;)</span></td>
      </tr>
      <tr>
        <td id="L33" class="blob-num js-line-number" data-line-number="33"></td>
        <td id="LC33" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L34" class="blob-num js-line-number" data-line-number="34"></td>
        <td id="LC34" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> set data dir, data used</span></td>
      </tr>
      <tr>
        <td id="L35" class="blob-num js-line-number" data-line-number="35"></td>
        <td id="LC35" class="blob-code blob-code-inner js-file-line">source(<span class="pl-s"><span class="pl-pds">&quot;</span>set-data-directory.R<span class="pl-pds">&quot;</span></span>)             <span class="pl-c"><span class="pl-c">#</span> data_dir must be first defined </span></td>
      </tr>
      <tr>
        <td id="L36" class="blob-num js-line-number" data-line-number="36"></td>
        <td id="LC36" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> alternative: give full path here, </span></td>
      </tr>
      <tr>
        <td id="L37" class="blob-num js-line-number" data-line-number="37"></td>
        <td id="LC37" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>            example data_dir=&quot;C:/Users/bekes.gabor/Dropbox (MTA KRTK)/bekes_kezdi_textbook/da_data_repo&quot;</span></td>
      </tr>
      <tr>
        <td id="L38" class="blob-num js-line-number" data-line-number="38"></td>
        <td id="LC38" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L39" class="blob-num js-line-number" data-line-number="39"></td>
        <td id="LC39" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L40" class="blob-num js-line-number" data-line-number="40"></td>
        <td id="LC40" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> load theme and functions</span></td>
      </tr>
      <tr>
        <td id="L41" class="blob-num js-line-number" data-line-number="41"></td>
        <td id="LC41" class="blob-code blob-code-inner js-file-line">source(<span class="pl-s"><span class="pl-pds">&quot;</span>ch00-tech-prep/theme_bg.R<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L42" class="blob-num js-line-number" data-line-number="42"></td>
        <td id="LC42" class="blob-code blob-code-inner js-file-line">source(<span class="pl-s"><span class="pl-pds">&quot;</span>ch00-tech-prep/da_helper_functions.R<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L43" class="blob-num js-line-number" data-line-number="43"></td>
        <td id="LC43" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L44" class="blob-num js-line-number" data-line-number="44"></td>
        <td id="LC44" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> set directory for code and data</span></td>
      </tr>
      <tr>
        <td id="L45" class="blob-num js-line-number" data-line-number="45"></td>
        <td id="LC45" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data_in</span> <span class="pl-k">&lt;-</span> paste(<span class="pl-smi">data_dir</span>,<span class="pl-s"><span class="pl-pds">&quot;</span>case-shiller-la<span class="pl-pds">&quot;</span></span>,<span class="pl-s"><span class="pl-pds">&quot;</span>clean/<span class="pl-pds">&quot;</span></span>, <span class="pl-v">sep</span> <span class="pl-k">=</span> <span class="pl-s"><span class="pl-pds">&quot;</span>/<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L46" class="blob-num js-line-number" data-line-number="46"></td>
        <td id="LC46" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">use_case_dir</span>          <span class="pl-k">&lt;-</span> <span class="pl-s"><span class="pl-pds">&quot;</span>ch18-case-shiller-la/<span class="pl-pds">&quot;</span></span></td>
      </tr>
      <tr>
        <td id="L47" class="blob-num js-line-number" data-line-number="47"></td>
        <td id="LC47" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L48" class="blob-num js-line-number" data-line-number="48"></td>
        <td id="LC48" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data_out</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">use_case_dir</span></td>
      </tr>
      <tr>
        <td id="L49" class="blob-num js-line-number" data-line-number="49"></td>
        <td id="LC49" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">output</span> <span class="pl-k">&lt;-</span> paste0(<span class="pl-smi">use_case_dir</span>,<span class="pl-s"><span class="pl-pds">&quot;</span>output/<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L50" class="blob-num js-line-number" data-line-number="50"></td>
        <td id="LC50" class="blob-code blob-code-inner js-file-line">create_output_if_doesnt_exist(<span class="pl-smi">output</span>)</td>
      </tr>
      <tr>
        <td id="L51" class="blob-num js-line-number" data-line-number="51"></td>
        <td id="LC51" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L52" class="blob-num js-line-number" data-line-number="52"></td>
        <td id="LC52" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L53" class="blob-num js-line-number" data-line-number="53"></td>
        <td id="LC53" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>-----------------------------------------------------------------------------------------</span></td>
      </tr>
      <tr>
        <td id="L54" class="blob-num js-line-number" data-line-number="54"></td>
        <td id="LC54" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>############################</span></td>
      </tr>
      <tr>
        <td id="L55" class="blob-num js-line-number" data-line-number="55"></td>
        <td id="LC55" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> RMSE functions</span></td>
      </tr>
      <tr>
        <td id="L56" class="blob-num js-line-number" data-line-number="56"></td>
        <td id="LC56" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>############################</span></td>
      </tr>
      <tr>
        <td id="L57" class="blob-num js-line-number" data-line-number="57"></td>
        <td id="LC57" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L58" class="blob-num js-line-number" data-line-number="58"></td>
        <td id="LC58" class="blob-code blob-code-inner js-file-line"><span class="pl-en">get_RMSE_from_model</span> <span class="pl-k">&lt;-</span> <span class="pl-k">function</span>(<span class="pl-smi">m</span>, <span class="pl-v">resid_col_name</span> <span class="pl-k">=</span> <span class="pl-s"><span class="pl-pds">&quot;</span>.resid<span class="pl-pds">&quot;</span></span>, <span class="pl-v">groupby</span> <span class="pl-k">=</span> c(<span class="pl-s"><span class="pl-pds">&quot;</span>.id<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>.model<span class="pl-pds">&quot;</span></span>)){</td>
      </tr>
      <tr>
        <td id="L59" class="blob-num js-line-number" data-line-number="59"></td>
        <td id="LC59" class="blob-code blob-code-inner js-file-line">  <span class="pl-smi">m</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L60" class="blob-num js-line-number" data-line-number="60"></td>
        <td id="LC60" class="blob-code blob-code-inner js-file-line">    residuals() %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L61" class="blob-num js-line-number" data-line-number="61"></td>
        <td id="LC61" class="blob-code blob-code-inner js-file-line">    as_tibble() %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L62" class="blob-num js-line-number" data-line-number="62"></td>
        <td id="LC62" class="blob-code blob-code-inner js-file-line">    group_by_at(<span class="pl-smi">groupby</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L63" class="blob-num js-line-number" data-line-number="63"></td>
        <td id="LC63" class="blob-code blob-code-inner js-file-line">    summarise(<span class="pl-v">RMSE</span> <span class="pl-k">=</span> mean(get(<span class="pl-smi">resid_col_name</span>)<span class="pl-k">**</span><span class="pl-c1">2</span>, <span class="pl-v">na.rm</span> <span class="pl-k">=</span> <span class="pl-c1">TRUE</span>)<span class="pl-k">**</span>(<span class="pl-c1">1</span><span class="pl-k">/</span><span class="pl-c1">2</span>))</td>
      </tr>
      <tr>
        <td id="L64" class="blob-num js-line-number" data-line-number="64"></td>
        <td id="LC64" class="blob-code blob-code-inner js-file-line">}</td>
      </tr>
      <tr>
        <td id="L65" class="blob-num js-line-number" data-line-number="65"></td>
        <td id="LC65" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L66" class="blob-num js-line-number" data-line-number="66"></td>
        <td id="LC66" class="blob-code blob-code-inner js-file-line"><span class="pl-en">get_MSE_from_forecast</span> <span class="pl-k">&lt;-</span> <span class="pl-k">function</span>(<span class="pl-smi">forecast</span>, <span class="pl-v">groupby</span> <span class="pl-k">=</span> c(<span class="pl-s"><span class="pl-pds">&quot;</span>.id<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>.model<span class="pl-pds">&quot;</span></span>)){</td>
      </tr>
      <tr>
        <td id="L67" class="blob-num js-line-number" data-line-number="67"></td>
        <td id="LC67" class="blob-code blob-code-inner js-file-line">  <span class="pl-smi">forecast</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L68" class="blob-num js-line-number" data-line-number="68"></td>
        <td id="LC68" class="blob-code blob-code-inner js-file-line">    as_tibble() %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L69" class="blob-num js-line-number" data-line-number="69"></td>
        <td id="LC69" class="blob-code blob-code-inner js-file-line">    group_by_at(<span class="pl-smi">groupby</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L70" class="blob-num js-line-number" data-line-number="70"></td>
        <td id="LC70" class="blob-code blob-code-inner js-file-line">    summarise(<span class="pl-v">MSE</span> <span class="pl-k">=</span> mean(<span class="pl-smi">e</span><span class="pl-k">^</span><span class="pl-c1">2</span>)) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L71" class="blob-num js-line-number" data-line-number="71"></td>
        <td id="LC71" class="blob-code blob-code-inner js-file-line">    ungroup()</td>
      </tr>
      <tr>
        <td id="L72" class="blob-num js-line-number" data-line-number="72"></td>
        <td id="LC72" class="blob-code blob-code-inner js-file-line">}</td>
      </tr>
      <tr>
        <td id="L73" class="blob-num js-line-number" data-line-number="73"></td>
        <td id="LC73" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L74" class="blob-num js-line-number" data-line-number="74"></td>
        <td id="LC74" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>-----------------------------------------------------------------------------------------</span></td>
      </tr>
      <tr>
        <td id="L75" class="blob-num js-line-number" data-line-number="75"></td>
        <td id="LC75" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L76" class="blob-num js-line-number" data-line-number="76"></td>
        <td id="LC76" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>############################</span></td>
      </tr>
      <tr>
        <td id="L77" class="blob-num js-line-number" data-line-number="77"></td>
        <td id="LC77" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> DATA PREP</span></td>
      </tr>
      <tr>
        <td id="L78" class="blob-num js-line-number" data-line-number="78"></td>
        <td id="LC78" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>############################</span></td>
      </tr>
      <tr>
        <td id="L79" class="blob-num js-line-number" data-line-number="79"></td>
        <td id="LC79" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>load data</span></td>
      </tr>
      <tr>
        <td id="L80" class="blob-num js-line-number" data-line-number="80"></td>
        <td id="LC80" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L81" class="blob-num js-line-number" data-line-number="81"></td>
        <td id="LC81" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data</span> <span class="pl-k">&lt;-</span> read_rds(paste0(<span class="pl-smi">data_in</span>,<span class="pl-s"><span class="pl-pds">&quot;</span>houseprices-data-1990-2018.rds<span class="pl-pds">&quot;</span></span>)) %<span class="pl-k">&gt;</span>% </td>
      </tr>
      <tr>
        <td id="L82" class="blob-num js-line-number" data-line-number="82"></td>
        <td id="LC82" class="blob-code blob-code-inner js-file-line">  mutate(<span class="pl-v">date</span> <span class="pl-k">=</span> as.Date(<span class="pl-smi">date</span>))</td>
      </tr>
      <tr>
        <td id="L83" class="blob-num js-line-number" data-line-number="83"></td>
        <td id="LC83" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L84" class="blob-num js-line-number" data-line-number="84"></td>
        <td id="LC84" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L85" class="blob-num js-line-number" data-line-number="85"></td>
        <td id="LC85" class="blob-code blob-code-inner js-file-line">  filter(<span class="pl-smi">date</span><span class="pl-k">&gt;</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span>2000-01-01<span class="pl-pds">&quot;</span></span> <span class="pl-k">&amp;</span> <span class="pl-smi">date</span><span class="pl-k">&lt;</span><span class="pl-s"><span class="pl-pds">&quot;</span>2018-01-01<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L86" class="blob-num js-line-number" data-line-number="86"></td>
        <td id="LC86" class="blob-code blob-code-inner js-file-line"> <span class="pl-c"><span class="pl-c">#</span> 18 years data</span></td>
      </tr>
      <tr>
        <td id="L87" class="blob-num js-line-number" data-line-number="87"></td>
        <td id="LC87" class="blob-code blob-code-inner js-file-line"> <span class="pl-c"><span class="pl-c">#</span> 1 year holdout</span></td>
      </tr>
      <tr>
        <td id="L88" class="blob-num js-line-number" data-line-number="88"></td>
        <td id="LC88" class="blob-code blob-code-inner js-file-line"> <span class="pl-c"><span class="pl-c">#</span> 4 years of test</span></td>
      </tr>
      <tr>
        <td id="L89" class="blob-num js-line-number" data-line-number="89"></td>
        <td id="LC89" class="blob-code blob-code-inner js-file-line"> <span class="pl-c"><span class="pl-c">#</span> 13 years of train (rolling window)</span></td>
      </tr>
      <tr>
        <td id="L90" class="blob-num js-line-number" data-line-number="90"></td>
        <td id="LC90" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L91" class="blob-num js-line-number" data-line-number="91"></td>
        <td id="LC91" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> pick if seasonal or non seasonal version used, will be cut later</span></td>
      </tr>
      <tr>
        <td id="L92" class="blob-num js-line-number" data-line-number="92"></td>
        <td id="LC92" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> here we pick pn, not seasonally adjusted</span></td>
      </tr>
      <tr>
        <td id="L93" class="blob-num js-line-number" data-line-number="93"></td>
        <td id="LC93" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L94" class="blob-num js-line-number" data-line-number="94"></td>
        <td id="LC94" class="blob-code blob-code-inner js-file-line">  mutate(</td>
      </tr>
      <tr>
        <td id="L95" class="blob-num js-line-number" data-line-number="95"></td>
        <td id="LC95" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">p</span><span class="pl-k">=</span><span class="pl-smi">pn</span>,</td>
      </tr>
      <tr>
        <td id="L96" class="blob-num js-line-number" data-line-number="96"></td>
        <td id="LC96" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">u</span><span class="pl-k">=</span><span class="pl-smi">us</span>,</td>
      </tr>
      <tr>
        <td id="L97" class="blob-num js-line-number" data-line-number="97"></td>
        <td id="LC97" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">emp</span><span class="pl-k">=</span><span class="pl-smi">emps</span></td>
      </tr>
      <tr>
        <td id="L98" class="blob-num js-line-number" data-line-number="98"></td>
        <td id="LC98" class="blob-code blob-code-inner js-file-line">      )</td>
      </tr>
      <tr>
        <td id="L99" class="blob-num js-line-number" data-line-number="99"></td>
        <td id="LC99" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L100" class="blob-num js-line-number" data-line-number="100"></td>
        <td id="LC100" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L101" class="blob-num js-line-number" data-line-number="101"></td>
        <td id="LC101" class="blob-code blob-code-inner js-file-line">  mutate(</td>
      </tr>
      <tr>
        <td id="L102" class="blob-num js-line-number" data-line-number="102"></td>
        <td id="LC102" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">dp</span>   <span class="pl-k">=</span> difference(<span class="pl-smi">p</span>, <span class="pl-v">lag</span><span class="pl-k">=</span><span class="pl-c1">1</span>, <span class="pl-v">order_by</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>),</td>
      </tr>
      <tr>
        <td id="L103" class="blob-num js-line-number" data-line-number="103"></td>
        <td id="LC103" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">p_lag</span> <span class="pl-k">=</span> lag(<span class="pl-smi">p</span>),</td>
      </tr>
      <tr>
        <td id="L104" class="blob-num js-line-number" data-line-number="104"></td>
        <td id="LC104" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">lnp</span> <span class="pl-k">=</span> log(<span class="pl-smi">p</span>),</td>
      </tr>
      <tr>
        <td id="L105" class="blob-num js-line-number" data-line-number="105"></td>
        <td id="LC105" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">dlnp</span>   <span class="pl-k">=</span> difference(<span class="pl-smi">lnp</span>, <span class="pl-v">lag</span><span class="pl-k">=</span><span class="pl-c1">1</span>, <span class="pl-v">order_by</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>),</td>
      </tr>
      <tr>
        <td id="L106" class="blob-num js-line-number" data-line-number="106"></td>
        <td id="LC106" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">lnp_lag</span> <span class="pl-k">=</span> lag(<span class="pl-smi">lnp</span>),</td>
      </tr>
      <tr>
        <td id="L107" class="blob-num js-line-number" data-line-number="107"></td>
        <td id="LC107" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">dlnp_lag</span>   <span class="pl-k">=</span> lag(<span class="pl-smi">dlnp</span>),</td>
      </tr>
      <tr>
        <td id="L108" class="blob-num js-line-number" data-line-number="108"></td>
        <td id="LC108" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">du</span>     <span class="pl-k">=</span> difference(<span class="pl-smi">u</span>, <span class="pl-v">lag</span><span class="pl-k">=</span><span class="pl-c1">1</span>, <span class="pl-v">order_by</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>),</td>
      </tr>
      <tr>
        <td id="L109" class="blob-num js-line-number" data-line-number="109"></td>
        <td id="LC109" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">lnemp</span> <span class="pl-k">=</span> log(<span class="pl-smi">emp</span>),</td>
      </tr>
      <tr>
        <td id="L110" class="blob-num js-line-number" data-line-number="110"></td>
        <td id="LC110" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">dlnemp</span> <span class="pl-k">=</span> difference(<span class="pl-smi">lnemp</span>, <span class="pl-v">lag</span><span class="pl-k">=</span><span class="pl-c1">1</span>, <span class="pl-v">order_by</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>)</td>
      </tr>
      <tr>
        <td id="L111" class="blob-num js-line-number" data-line-number="111"></td>
        <td id="LC111" class="blob-code blob-code-inner js-file-line">  )</td>
      </tr>
      <tr>
        <td id="L112" class="blob-num js-line-number" data-line-number="112"></td>
        <td id="LC112" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L113" class="blob-num js-line-number" data-line-number="113"></td>
        <td id="LC113" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>############################</span></td>
      </tr>
      <tr>
        <td id="L114" class="blob-num js-line-number" data-line-number="114"></td>
        <td id="LC114" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> GRAPHS part 1</span></td>
      </tr>
      <tr>
        <td id="L115" class="blob-num js-line-number" data-line-number="115"></td>
        <td id="LC115" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>############################</span></td>
      </tr>
      <tr>
        <td id="L116" class="blob-num js-line-number" data-line-number="116"></td>
        <td id="LC116" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L117" class="blob-num js-line-number" data-line-number="117"></td>
        <td id="LC117" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L118" class="blob-num js-line-number" data-line-number="118"></td>
        <td id="LC118" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L119" class="blob-num js-line-number" data-line-number="119"></td>
        <td id="LC119" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Plot price index</span></td>
      </tr>
      <tr>
        <td id="L120" class="blob-num js-line-number" data-line-number="120"></td>
        <td id="LC120" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">price_index_plot</span> <span class="pl-k">&lt;-</span> ggplot(<span class="pl-v">data</span> <span class="pl-k">=</span> <span class="pl-smi">data</span>, aes(<span class="pl-v">x</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>, <span class="pl-v">y</span> <span class="pl-k">=</span> <span class="pl-smi">p</span>))<span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L121" class="blob-num js-line-number" data-line-number="121"></td>
        <td id="LC121" class="blob-code blob-code-inner js-file-line">  geom_line_da() <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L122" class="blob-num js-line-number" data-line-number="122"></td>
        <td id="LC122" class="blob-code blob-code-inner js-file-line">  ylab(<span class="pl-s"><span class="pl-pds">&quot;</span>Case-shiller Price index<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L123" class="blob-num js-line-number" data-line-number="123"></td>
        <td id="LC123" class="blob-code blob-code-inner js-file-line">  xlab(<span class="pl-s"><span class="pl-pds">&quot;</span>Date (month)<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L124" class="blob-num js-line-number" data-line-number="124"></td>
        <td id="LC124" class="blob-code blob-code-inner js-file-line">  scale_y_continuous(<span class="pl-v">limits</span> <span class="pl-k">=</span> c(<span class="pl-c1">50</span>,<span class="pl-c1">300</span>), <span class="pl-v">breaks</span> <span class="pl-k">=</span> seq(<span class="pl-c1">50</span>,<span class="pl-c1">300</span>,<span class="pl-c1">50</span>)) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L125" class="blob-num js-line-number" data-line-number="125"></td>
        <td id="LC125" class="blob-code blob-code-inner js-file-line">  scale_x_date(<span class="pl-v">expand</span> <span class="pl-k">=</span> c(<span class="pl-c1">0.01</span>, <span class="pl-c1">0.01</span>),   <span class="pl-v">breaks</span> <span class="pl-k">=</span> as.Date(c(<span class="pl-s"><span class="pl-pds">&quot;</span>2000-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2003-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2006-01-01<span class="pl-pds">&quot;</span></span>,  <span class="pl-s"><span class="pl-pds">&quot;</span>2009-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2012-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2015-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2018-01-01<span class="pl-pds">&quot;</span></span>)),  <span class="pl-v">labels</span> <span class="pl-k">=</span> date_format(<span class="pl-s"><span class="pl-pds">&quot;</span>%b%Y<span class="pl-pds">&quot;</span></span>)) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L126" class="blob-num js-line-number" data-line-number="126"></td>
        <td id="LC126" class="blob-code blob-code-inner js-file-line">  theme_bg()</td>
      </tr>
      <tr>
        <td id="L127" class="blob-num js-line-number" data-line-number="127"></td>
        <td id="LC127" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">price_index_plot</span></td>
      </tr>
      <tr>
        <td id="L128" class="blob-num js-line-number" data-line-number="128"></td>
        <td id="LC128" class="blob-code blob-code-inner js-file-line">save_fig(<span class="pl-s"><span class="pl-pds">&quot;</span>ch18-figure-8-cs-tseries-p<span class="pl-pds">&quot;</span></span>, <span class="pl-smi">output</span>, <span class="pl-s"><span class="pl-pds">&quot;</span>small<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L129" class="blob-num js-line-number" data-line-number="129"></td>
        <td id="LC129" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L130" class="blob-num js-line-number" data-line-number="130"></td>
        <td id="LC130" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L131" class="blob-num js-line-number" data-line-number="131"></td>
        <td id="LC131" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L132" class="blob-num js-line-number" data-line-number="132"></td>
        <td id="LC132" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L133" class="blob-num js-line-number" data-line-number="133"></td>
        <td id="LC133" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L134" class="blob-num js-line-number" data-line-number="134"></td>
        <td id="LC134" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Plot log difference of price index</span></td>
      </tr>
      <tr>
        <td id="L135" class="blob-num js-line-number" data-line-number="135"></td>
        <td id="LC135" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">dp_plot</span> <span class="pl-k">&lt;-</span> ggplot(<span class="pl-v">data</span> <span class="pl-k">=</span> <span class="pl-smi">data</span>, aes(<span class="pl-v">x</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>, <span class="pl-v">y</span> <span class="pl-k">=</span> <span class="pl-smi">dp</span>))<span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L136" class="blob-num js-line-number" data-line-number="136"></td>
        <td id="LC136" class="blob-code blob-code-inner js-file-line">  geom_line_da() <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L137" class="blob-num js-line-number" data-line-number="137"></td>
        <td id="LC137" class="blob-code blob-code-inner js-file-line">  ylab(<span class="pl-s"><span class="pl-pds">&quot;</span>First difference of price index<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L138" class="blob-num js-line-number" data-line-number="138"></td>
        <td id="LC138" class="blob-code blob-code-inner js-file-line">  xlab(<span class="pl-s"><span class="pl-pds">&quot;</span>Date (month)<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L139" class="blob-num js-line-number" data-line-number="139"></td>
        <td id="LC139" class="blob-code blob-code-inner js-file-line">  scale_y_continuous(<span class="pl-v">limits</span> <span class="pl-k">=</span> c(<span class="pl-k">-</span><span class="pl-c1">10</span>,<span class="pl-c1">8</span>), <span class="pl-v">breaks</span> <span class="pl-k">=</span> seq(<span class="pl-k">-</span><span class="pl-c1">10</span>,<span class="pl-c1">8</span>,<span class="pl-c1">2</span>)) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L140" class="blob-num js-line-number" data-line-number="140"></td>
        <td id="LC140" class="blob-code blob-code-inner js-file-line">  scale_x_date(<span class="pl-v">expand</span> <span class="pl-k">=</span> c(<span class="pl-c1">0.01</span>, <span class="pl-c1">0.01</span>),   <span class="pl-v">breaks</span> <span class="pl-k">=</span> as.Date(c(<span class="pl-s"><span class="pl-pds">&quot;</span>2000-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2003-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2006-01-01<span class="pl-pds">&quot;</span></span>,  <span class="pl-s"><span class="pl-pds">&quot;</span>2009-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2012-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2015-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2018-01-01<span class="pl-pds">&quot;</span></span>)),  <span class="pl-v">labels</span> <span class="pl-k">=</span> date_format(<span class="pl-s"><span class="pl-pds">&quot;</span>%b%Y<span class="pl-pds">&quot;</span></span>)) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L141" class="blob-num js-line-number" data-line-number="141"></td>
        <td id="LC141" class="blob-code blob-code-inner js-file-line">  theme_bg()</td>
      </tr>
      <tr>
        <td id="L142" class="blob-num js-line-number" data-line-number="142"></td>
        <td id="LC142" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">dp_plot</span></td>
      </tr>
      <tr>
        <td id="L143" class="blob-num js-line-number" data-line-number="143"></td>
        <td id="LC143" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>save_fig(&quot;cs_tseries_dp_R&quot;, output, &quot;small&quot;)</span></td>
      </tr>
      <tr>
        <td id="L144" class="blob-num js-line-number" data-line-number="144"></td>
        <td id="LC144" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L145" class="blob-num js-line-number" data-line-number="145"></td>
        <td id="LC145" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L146" class="blob-num js-line-number" data-line-number="146"></td>
        <td id="LC146" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Plot log difference of price index</span></td>
      </tr>
      <tr>
        <td id="L147" class="blob-num js-line-number" data-line-number="147"></td>
        <td id="LC147" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">dlnp_plot</span> <span class="pl-k">&lt;-</span> ggplot(<span class="pl-v">data</span> <span class="pl-k">=</span> <span class="pl-smi">data</span>, aes(<span class="pl-v">x</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>, <span class="pl-v">y</span> <span class="pl-k">=</span> <span class="pl-smi">dlnp</span>))<span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L148" class="blob-num js-line-number" data-line-number="148"></td>
        <td id="LC148" class="blob-code blob-code-inner js-file-line">  geom_line_da() <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L149" class="blob-num js-line-number" data-line-number="149"></td>
        <td id="LC149" class="blob-code blob-code-inner js-file-line">  ylab(<span class="pl-s"><span class="pl-pds">&quot;</span>Log first difference of price index<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L150" class="blob-num js-line-number" data-line-number="150"></td>
        <td id="LC150" class="blob-code blob-code-inner js-file-line">  xlab(<span class="pl-s"><span class="pl-pds">&quot;</span>Date (month)<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L151" class="blob-num js-line-number" data-line-number="151"></td>
        <td id="LC151" class="blob-code blob-code-inner js-file-line">  scale_y_continuous(<span class="pl-v">limits</span> <span class="pl-k">=</span> c(<span class="pl-k">-</span><span class="pl-c1">0.04</span>,<span class="pl-c1">0.04</span>), <span class="pl-v">breaks</span> <span class="pl-k">=</span> seq(<span class="pl-k">-</span><span class="pl-c1">0.04</span>,<span class="pl-c1">0.04</span>,<span class="pl-c1">0.01</span>)) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L152" class="blob-num js-line-number" data-line-number="152"></td>
        <td id="LC152" class="blob-code blob-code-inner js-file-line">  scale_x_date(<span class="pl-v">date_breaks</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span>2 years<span class="pl-pds">&quot;</span></span>, <span class="pl-v">labels</span> <span class="pl-k">=</span> date_format(<span class="pl-s"><span class="pl-pds">&quot;</span>%b%Y<span class="pl-pds">&quot;</span></span>)) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L153" class="blob-num js-line-number" data-line-number="153"></td>
        <td id="LC153" class="blob-code blob-code-inner js-file-line">  theme_bg()</td>
      </tr>
      <tr>
        <td id="L154" class="blob-num js-line-number" data-line-number="154"></td>
        <td id="LC154" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">dlnp_plot</span></td>
      </tr>
      <tr>
        <td id="L155" class="blob-num js-line-number" data-line-number="155"></td>
        <td id="LC155" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>save_fig(&quot;cs_tseries_dlnp_R&quot;, output, &quot;small&quot;)</span></td>
      </tr>
      <tr>
        <td id="L156" class="blob-num js-line-number" data-line-number="156"></td>
        <td id="LC156" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L157" class="blob-num js-line-number" data-line-number="157"></td>
        <td id="LC157" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L158" class="blob-num js-line-number" data-line-number="158"></td>
        <td id="LC158" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L159" class="blob-num js-line-number" data-line-number="159"></td>
        <td id="LC159" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>############################</span></td>
      </tr>
      <tr>
        <td id="L160" class="blob-num js-line-number" data-line-number="160"></td>
        <td id="LC160" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> GRAPHS part 2</span></td>
      </tr>
      <tr>
        <td id="L161" class="blob-num js-line-number" data-line-number="161"></td>
        <td id="LC161" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>############################</span></td>
      </tr>
      <tr>
        <td id="L162" class="blob-num js-line-number" data-line-number="162"></td>
        <td id="LC162" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L163" class="blob-num js-line-number" data-line-number="163"></td>
        <td id="LC163" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Plot employment</span></td>
      </tr>
      <tr>
        <td id="L164" class="blob-num js-line-number" data-line-number="164"></td>
        <td id="LC164" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">emp_plot</span><span class="pl-k">&lt;-</span>ggplot(<span class="pl-v">data</span> <span class="pl-k">=</span> <span class="pl-smi">data</span>, aes(<span class="pl-v">x</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>, <span class="pl-v">y</span> <span class="pl-k">=</span> <span class="pl-smi">emp</span>))<span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L165" class="blob-num js-line-number" data-line-number="165"></td>
        <td id="LC165" class="blob-code blob-code-inner js-file-line">  geom_line_da() <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L166" class="blob-num js-line-number" data-line-number="166"></td>
        <td id="LC166" class="blob-code blob-code-inner js-file-line">  ylab(<span class="pl-s"><span class="pl-pds">&quot;</span>Employment (in thousands)<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L167" class="blob-num js-line-number" data-line-number="167"></td>
        <td id="LC167" class="blob-code blob-code-inner js-file-line">  xlab(<span class="pl-s"><span class="pl-pds">&quot;</span>Date (month)<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L168" class="blob-num js-line-number" data-line-number="168"></td>
        <td id="LC168" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>  scale_y_continuous(limits = c(10000,18000), breaks = seq(10000,18000,2000)) +</span></td>
      </tr>
      <tr>
        <td id="L169" class="blob-num js-line-number" data-line-number="169"></td>
        <td id="LC169" class="blob-code blob-code-inner js-file-line">  scale_x_date(<span class="pl-v">expand</span> <span class="pl-k">=</span> c(<span class="pl-c1">0.01</span>, <span class="pl-c1">0.01</span>),   <span class="pl-v">breaks</span> <span class="pl-k">=</span> as.Date(c(<span class="pl-s"><span class="pl-pds">&quot;</span>2000-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2003-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2006-01-01<span class="pl-pds">&quot;</span></span>,  <span class="pl-s"><span class="pl-pds">&quot;</span>2009-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2012-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2015-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2018-01-01<span class="pl-pds">&quot;</span></span>)),  <span class="pl-v">labels</span> <span class="pl-k">=</span> date_format(<span class="pl-s"><span class="pl-pds">&quot;</span>%b%Y<span class="pl-pds">&quot;</span></span>)) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L170" class="blob-num js-line-number" data-line-number="170"></td>
        <td id="LC170" class="blob-code blob-code-inner js-file-line">  theme_bg()</td>
      </tr>
      <tr>
        <td id="L171" class="blob-num js-line-number" data-line-number="171"></td>
        <td id="LC171" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">emp_plot</span></td>
      </tr>
      <tr>
        <td id="L172" class="blob-num js-line-number" data-line-number="172"></td>
        <td id="LC172" class="blob-code blob-code-inner js-file-line">save_fig(<span class="pl-s"><span class="pl-pds">&quot;</span>ch18-figure-10c-cs-tseries-emp<span class="pl-pds">&quot;</span></span>, <span class="pl-smi">output</span>, <span class="pl-s"><span class="pl-pds">&quot;</span>small<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L173" class="blob-num js-line-number" data-line-number="173"></td>
        <td id="LC173" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L174" class="blob-num js-line-number" data-line-number="174"></td>
        <td id="LC174" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L175" class="blob-num js-line-number" data-line-number="175"></td>
        <td id="LC175" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Plot log diff employment</span></td>
      </tr>
      <tr>
        <td id="L176" class="blob-num js-line-number" data-line-number="176"></td>
        <td id="LC176" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">ldemp_plot</span><span class="pl-k">&lt;-</span> ggplot(<span class="pl-v">data</span> <span class="pl-k">=</span> <span class="pl-smi">data</span>, aes(<span class="pl-v">x</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>, <span class="pl-v">y</span> <span class="pl-k">=</span> <span class="pl-smi">dlnemp</span>))<span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L177" class="blob-num js-line-number" data-line-number="177"></td>
        <td id="LC177" class="blob-code blob-code-inner js-file-line">  geom_line_da() <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L178" class="blob-num js-line-number" data-line-number="178"></td>
        <td id="LC178" class="blob-code blob-code-inner js-file-line">  ylab(<span class="pl-s"><span class="pl-pds">&quot;</span>Log change in employment<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L179" class="blob-num js-line-number" data-line-number="179"></td>
        <td id="LC179" class="blob-code blob-code-inner js-file-line">  xlab(<span class="pl-s"><span class="pl-pds">&quot;</span>Date (month)<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L180" class="blob-num js-line-number" data-line-number="180"></td>
        <td id="LC180" class="blob-code blob-code-inner js-file-line">  scale_x_date(<span class="pl-v">expand</span> <span class="pl-k">=</span> c(<span class="pl-c1">0.01</span>, <span class="pl-c1">0.01</span>),   <span class="pl-v">breaks</span> <span class="pl-k">=</span> as.Date(c(<span class="pl-s"><span class="pl-pds">&quot;</span>2000-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2003-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2006-01-01<span class="pl-pds">&quot;</span></span>,  <span class="pl-s"><span class="pl-pds">&quot;</span>2009-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2012-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2015-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2018-01-01<span class="pl-pds">&quot;</span></span>)),  <span class="pl-v">labels</span> <span class="pl-k">=</span> date_format(<span class="pl-s"><span class="pl-pds">&quot;</span>%b%Y<span class="pl-pds">&quot;</span></span>)) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L181" class="blob-num js-line-number" data-line-number="181"></td>
        <td id="LC181" class="blob-code blob-code-inner js-file-line">  theme_bg()</td>
      </tr>
      <tr>
        <td id="L182" class="blob-num js-line-number" data-line-number="182"></td>
        <td id="LC182" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">ldemp_plot</span></td>
      </tr>
      <tr>
        <td id="L183" class="blob-num js-line-number" data-line-number="183"></td>
        <td id="LC183" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>save_fig(&quot;cs_tseries_dlnemp_R&quot;, output, &quot;small&quot;)</span></td>
      </tr>
      <tr>
        <td id="L184" class="blob-num js-line-number" data-line-number="184"></td>
        <td id="LC184" class="blob-code blob-code-inner js-file-line">save_fig(<span class="pl-s"><span class="pl-pds">&quot;</span>ch18-figure-10d-cs-tseries-dlnemp<span class="pl-pds">&quot;</span></span>, <span class="pl-smi">output</span>, <span class="pl-s"><span class="pl-pds">&quot;</span>small<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L185" class="blob-num js-line-number" data-line-number="185"></td>
        <td id="LC185" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L186" class="blob-num js-line-number" data-line-number="186"></td>
        <td id="LC186" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L187" class="blob-num js-line-number" data-line-number="187"></td>
        <td id="LC187" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Plot unemplyiment rate</span></td>
      </tr>
      <tr>
        <td id="L188" class="blob-num js-line-number" data-line-number="188"></td>
        <td id="LC188" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">u_plot</span><span class="pl-k">&lt;-</span>ggplot(<span class="pl-v">data</span> <span class="pl-k">=</span> <span class="pl-smi">data</span>, aes(<span class="pl-v">x</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>, <span class="pl-v">y</span> <span class="pl-k">=</span> <span class="pl-smi">u</span>))<span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L189" class="blob-num js-line-number" data-line-number="189"></td>
        <td id="LC189" class="blob-code blob-code-inner js-file-line">  geom_line_da() <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L190" class="blob-num js-line-number" data-line-number="190"></td>
        <td id="LC190" class="blob-code blob-code-inner js-file-line">  ylab(<span class="pl-s"><span class="pl-pds">&quot;</span>Unemployment rate (percent)<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L191" class="blob-num js-line-number" data-line-number="191"></td>
        <td id="LC191" class="blob-code blob-code-inner js-file-line">  xlab(<span class="pl-s"><span class="pl-pds">&quot;</span>Date (month)<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L192" class="blob-num js-line-number" data-line-number="192"></td>
        <td id="LC192" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>  scale_y_continuous(limits = c(10000,18000), breaks = seq(10000,18000,2000)) +</span></td>
      </tr>
      <tr>
        <td id="L193" class="blob-num js-line-number" data-line-number="193"></td>
        <td id="LC193" class="blob-code blob-code-inner js-file-line">  scale_x_date(<span class="pl-v">expand</span> <span class="pl-k">=</span> c(<span class="pl-c1">0.01</span>, <span class="pl-c1">0.01</span>),   <span class="pl-v">breaks</span> <span class="pl-k">=</span> as.Date(c(<span class="pl-s"><span class="pl-pds">&quot;</span>2000-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2003-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2006-01-01<span class="pl-pds">&quot;</span></span>,  <span class="pl-s"><span class="pl-pds">&quot;</span>2009-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2012-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2015-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2018-01-01<span class="pl-pds">&quot;</span></span>)),  <span class="pl-v">labels</span> <span class="pl-k">=</span> date_format(<span class="pl-s"><span class="pl-pds">&quot;</span>%b%Y<span class="pl-pds">&quot;</span></span>)) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L194" class="blob-num js-line-number" data-line-number="194"></td>
        <td id="LC194" class="blob-code blob-code-inner js-file-line">  theme_bg()</td>
      </tr>
      <tr>
        <td id="L195" class="blob-num js-line-number" data-line-number="195"></td>
        <td id="LC195" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">u_plot</span></td>
      </tr>
      <tr>
        <td id="L196" class="blob-num js-line-number" data-line-number="196"></td>
        <td id="LC196" class="blob-code blob-code-inner js-file-line">save_fig(<span class="pl-s"><span class="pl-pds">&quot;</span>ch18-figure-10a-cs-tseries-u<span class="pl-pds">&quot;</span></span>, <span class="pl-smi">output</span>, <span class="pl-s"><span class="pl-pds">&quot;</span>small<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L197" class="blob-num js-line-number" data-line-number="197"></td>
        <td id="LC197" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L198" class="blob-num js-line-number" data-line-number="198"></td>
        <td id="LC198" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L199" class="blob-num js-line-number" data-line-number="199"></td>
        <td id="LC199" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Plot diff unemployment</span></td>
      </tr>
      <tr>
        <td id="L200" class="blob-num js-line-number" data-line-number="200"></td>
        <td id="LC200" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">du_plot</span><span class="pl-k">&lt;-</span> ggplot(<span class="pl-v">data</span> <span class="pl-k">=</span> <span class="pl-smi">data</span>, aes(<span class="pl-v">x</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>, <span class="pl-v">y</span> <span class="pl-k">=</span> <span class="pl-smi">du</span>))<span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L201" class="blob-num js-line-number" data-line-number="201"></td>
        <td id="LC201" class="blob-code blob-code-inner js-file-line">  geom_line_da() <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L202" class="blob-num js-line-number" data-line-number="202"></td>
        <td id="LC202" class="blob-code blob-code-inner js-file-line">  ylab(<span class="pl-s"><span class="pl-pds">&quot;</span>Change in unemployment rate<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L203" class="blob-num js-line-number" data-line-number="203"></td>
        <td id="LC203" class="blob-code blob-code-inner js-file-line">  xlab(<span class="pl-s"><span class="pl-pds">&quot;</span>Date (month)<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L204" class="blob-num js-line-number" data-line-number="204"></td>
        <td id="LC204" class="blob-code blob-code-inner js-file-line">  scale_x_date(<span class="pl-v">expand</span> <span class="pl-k">=</span> c(<span class="pl-c1">0.01</span>, <span class="pl-c1">0.01</span>),   <span class="pl-v">breaks</span> <span class="pl-k">=</span> as.Date(c(<span class="pl-s"><span class="pl-pds">&quot;</span>2000-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2003-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2006-01-01<span class="pl-pds">&quot;</span></span>,  <span class="pl-s"><span class="pl-pds">&quot;</span>2009-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2012-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2015-01-01<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>2018-01-01<span class="pl-pds">&quot;</span></span>)),  <span class="pl-v">labels</span> <span class="pl-k">=</span> date_format(<span class="pl-s"><span class="pl-pds">&quot;</span>%b%Y<span class="pl-pds">&quot;</span></span>)) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L205" class="blob-num js-line-number" data-line-number="205"></td>
        <td id="LC205" class="blob-code blob-code-inner js-file-line">  theme_bg()</td>
      </tr>
      <tr>
        <td id="L206" class="blob-num js-line-number" data-line-number="206"></td>
        <td id="LC206" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">du_plot</span></td>
      </tr>
      <tr>
        <td id="L207" class="blob-num js-line-number" data-line-number="207"></td>
        <td id="LC207" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>save_fig(&quot;cs_tseries_du_R&quot;, output, &quot;small&quot;)</span></td>
      </tr>
      <tr>
        <td id="L208" class="blob-num js-line-number" data-line-number="208"></td>
        <td id="LC208" class="blob-code blob-code-inner js-file-line">save_fig(<span class="pl-s"><span class="pl-pds">&quot;</span>ch18-figure-10b-cs-tseries-du<span class="pl-pds">&quot;</span></span>, <span class="pl-smi">output</span>, <span class="pl-s"><span class="pl-pds">&quot;</span>small<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L209" class="blob-num js-line-number" data-line-number="209"></td>
        <td id="LC209" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L210" class="blob-num js-line-number" data-line-number="210"></td>
        <td id="LC210" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L211" class="blob-num js-line-number" data-line-number="211"></td>
        <td id="LC211" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L212" class="blob-num js-line-number" data-line-number="212"></td>
        <td id="LC212" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>############################</span></td>
      </tr>
      <tr>
        <td id="L213" class="blob-num js-line-number" data-line-number="213"></td>
        <td id="LC213" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Create train/houldout data</span></td>
      </tr>
      <tr>
        <td id="L214" class="blob-num js-line-number" data-line-number="214"></td>
        <td id="LC214" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>############################</span></td>
      </tr>
      <tr>
        <td id="L215" class="blob-num js-line-number" data-line-number="215"></td>
        <td id="LC215" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L216" class="blob-num js-line-number" data-line-number="216"></td>
        <td id="LC216" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Last year of data</span></td>
      </tr>
      <tr>
        <td id="L217" class="blob-num js-line-number" data-line-number="217"></td>
        <td id="LC217" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data_holdout</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L218" class="blob-num js-line-number" data-line-number="218"></td>
        <td id="LC218" class="blob-code blob-code-inner js-file-line">  slice((n()<span class="pl-k">-</span><span class="pl-c1">11</span>)<span class="pl-k">:</span>n())</td>
      </tr>
      <tr>
        <td id="L219" class="blob-num js-line-number" data-line-number="219"></td>
        <td id="LC219" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L220" class="blob-num js-line-number" data-line-number="220"></td>
        <td id="LC220" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Rest of data for training</span></td>
      </tr>
      <tr>
        <td id="L221" class="blob-num js-line-number" data-line-number="221"></td>
        <td id="LC221" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data_train</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L222" class="blob-num js-line-number" data-line-number="222"></td>
        <td id="LC222" class="blob-code blob-code-inner js-file-line">  slice(<span class="pl-c1">1</span><span class="pl-k">:</span>(n()<span class="pl-k">-</span><span class="pl-c1">12</span>))</td>
      </tr>
      <tr>
        <td id="L223" class="blob-num js-line-number" data-line-number="223"></td>
        <td id="LC223" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L224" class="blob-num js-line-number" data-line-number="224"></td>
        <td id="LC224" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Prepare for cross-validation, define size of train</span></td>
      </tr>
      <tr>
        <td id="L225" class="blob-num js-line-number" data-line-number="225"></td>
        <td id="LC225" class="blob-code blob-code-inner js-file-line"><span class="pl-v">train_legth</span><span class="pl-k">=</span><span class="pl-c1">13</span></td>
      </tr>
      <tr>
        <td id="L226" class="blob-num js-line-number" data-line-number="226"></td>
        <td id="LC226" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data_tr</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data_train</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L227" class="blob-num js-line-number" data-line-number="227"></td>
        <td id="LC227" class="blob-code blob-code-inner js-file-line">  slice(<span class="pl-c1">1</span><span class="pl-k">:</span>(n()<span class="pl-k">-</span><span class="pl-c1">12</span>)) %<span class="pl-k">&gt;</span>% <span class="pl-c"><span class="pl-c">#</span> last year of training data not used in any fold as training</span></td>
      </tr>
      <tr>
        <td id="L228" class="blob-num js-line-number" data-line-number="228"></td>
        <td id="LC228" class="blob-code blob-code-inner js-file-line">  slide_tsibble(<span class="pl-v">.size</span> <span class="pl-k">=</span> <span class="pl-smi">train_legth</span><span class="pl-k">*</span><span class="pl-c1">12</span>, <span class="pl-v">.step</span> <span class="pl-k">=</span> <span class="pl-c1">12</span>)</td>
      </tr>
      <tr>
        <td id="L229" class="blob-num js-line-number" data-line-number="229"></td>
        <td id="LC229" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L230" class="blob-num js-line-number" data-line-number="230"></td>
        <td id="LC230" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>####################################</span></td>
      </tr>
      <tr>
        <td id="L231" class="blob-num js-line-number" data-line-number="231"></td>
        <td id="LC231" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Look at some TS regressions, tests</span></td>
      </tr>
      <tr>
        <td id="L232" class="blob-num js-line-number" data-line-number="232"></td>
        <td id="LC232" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>####################################</span></td>
      </tr>
      <tr>
        <td id="L233" class="blob-num js-line-number" data-line-number="233"></td>
        <td id="LC233" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L234" class="blob-num js-line-number" data-line-number="234"></td>
        <td id="LC234" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> test unit root</span></td>
      </tr>
      <tr>
        <td id="L235" class="blob-num js-line-number" data-line-number="235"></td>
        <td id="LC235" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L236" class="blob-num js-line-number" data-line-number="236"></td>
        <td id="LC236" class="blob-code blob-code-inner js-file-line">  features(<span class="pl-smi">p</span>, <span class="pl-smi">unitroot_kpss</span>)</td>
      </tr>
      <tr>
        <td id="L237" class="blob-num js-line-number" data-line-number="237"></td>
        <td id="LC237" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L238" class="blob-num js-line-number" data-line-number="238"></td>
        <td id="LC238" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L239" class="blob-num js-line-number" data-line-number="239"></td>
        <td id="LC239" class="blob-code blob-code-inner js-file-line">  features(<span class="pl-smi">lnp</span>, <span class="pl-smi">unitroot_kpss</span>)</td>
      </tr>
      <tr>
        <td id="L240" class="blob-num js-line-number" data-line-number="240"></td>
        <td id="LC240" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L241" class="blob-num js-line-number" data-line-number="241"></td>
        <td id="LC241" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Look at model coefficients on train data to see if seasonality matters</span></td>
      </tr>
      <tr>
        <td id="L242" class="blob-num js-line-number" data-line-number="242"></td>
        <td id="LC242" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">fit</span> <span class="pl-k">&lt;-</span><span class="pl-smi">data_train</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L243" class="blob-num js-line-number" data-line-number="243"></td>
        <td id="LC243" class="blob-code blob-code-inner js-file-line">  model(</td>
      </tr>
      <tr>
        <td id="L244" class="blob-num js-line-number" data-line-number="244"></td>
        <td id="LC244" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">m5</span> <span class="pl-k">=</span>  ARIMA(<span class="pl-smi">p</span> <span class="pl-k">~</span> <span class="pl-c1">1</span> <span class="pl-k">+</span>season() <span class="pl-k">+</span>  PDQ(<span class="pl-c1">0</span>,<span class="pl-c1">0</span>,<span class="pl-c1">0</span>))</td>
      </tr>
      <tr>
        <td id="L245" class="blob-num js-line-number" data-line-number="245"></td>
        <td id="LC245" class="blob-code blob-code-inner js-file-line">  )</td>
      </tr>
      <tr>
        <td id="L246" class="blob-num js-line-number" data-line-number="246"></td>
        <td id="LC246" class="blob-code blob-code-inner js-file-line">report(<span class="pl-smi">fit</span>)</td>
      </tr>
      <tr>
        <td id="L247" class="blob-num js-line-number" data-line-number="247"></td>
        <td id="LC247" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L248" class="blob-num js-line-number" data-line-number="248"></td>
        <td id="LC248" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">fit2</span> <span class="pl-k">&lt;-</span><span class="pl-smi">data_train</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L249" class="blob-num js-line-number" data-line-number="249"></td>
        <td id="LC249" class="blob-code blob-code-inner js-file-line">  model(</td>
      </tr>
      <tr>
        <td id="L250" class="blob-num js-line-number" data-line-number="250"></td>
        <td id="LC250" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">m5</span> <span class="pl-k">=</span>  ARIMA(<span class="pl-smi">dp</span> <span class="pl-k">~</span>  <span class="pl-c1">1</span> <span class="pl-k">+</span> season()<span class="pl-k">+</span> PDQ(<span class="pl-c1">0</span>,<span class="pl-c1">0</span>,<span class="pl-c1">0</span>))</td>
      </tr>
      <tr>
        <td id="L251" class="blob-num js-line-number" data-line-number="251"></td>
        <td id="LC251" class="blob-code blob-code-inner js-file-line">  )</td>
      </tr>
      <tr>
        <td id="L252" class="blob-num js-line-number" data-line-number="252"></td>
        <td id="LC252" class="blob-code blob-code-inner js-file-line">report(<span class="pl-smi">fit2</span>)</td>
      </tr>
      <tr>
        <td id="L253" class="blob-num js-line-number" data-line-number="253"></td>
        <td id="LC253" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L254" class="blob-num js-line-number" data-line-number="254"></td>
        <td id="LC254" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> AR(1) is like simple regression</span></td>
      </tr>
      <tr>
        <td id="L255" class="blob-num js-line-number" data-line-number="255"></td>
        <td id="LC255" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">r</span><span class="pl-k">&lt;-</span> lm(<span class="pl-v">data</span><span class="pl-k">=</span><span class="pl-smi">data</span>, <span class="pl-smi">dlnp</span><span class="pl-k">~</span> <span class="pl-smi">dlnp_lag</span>)</td>
      </tr>
      <tr>
        <td id="L256" class="blob-num js-line-number" data-line-number="256"></td>
        <td id="LC256" class="blob-code blob-code-inner js-file-line">summary(<span class="pl-smi">r</span>)</td>
      </tr>
      <tr>
        <td id="L257" class="blob-num js-line-number" data-line-number="257"></td>
        <td id="LC257" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L258" class="blob-num js-line-number" data-line-number="258"></td>
        <td id="LC258" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L259" class="blob-num js-line-number" data-line-number="259"></td>
        <td id="LC259" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>############################</span></td>
      </tr>
      <tr>
        <td id="L260" class="blob-num js-line-number" data-line-number="260"></td>
        <td id="LC260" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Fit ARIMA type models</span></td>
      </tr>
      <tr>
        <td id="L261" class="blob-num js-line-number" data-line-number="261"></td>
        <td id="LC261" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>############################</span></td>
      </tr>
      <tr>
        <td id="L262" class="blob-num js-line-number" data-line-number="262"></td>
        <td id="LC262" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> To cross-validate auto.arima,</span></td>
      </tr>
      <tr>
        <td id="L263" class="blob-num js-line-number" data-line-number="263"></td>
        <td id="LC263" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> step 1: run it and find ARIMA specification on the whole train data, p,q chosen by BIC</span></td>
      </tr>
      <tr>
        <td id="L264" class="blob-num js-line-number" data-line-number="264"></td>
        <td id="LC264" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> step 2: use the selected model as a candidate</span></td>
      </tr>
      <tr>
        <td id="L265" class="blob-num js-line-number" data-line-number="265"></td>
        <td id="LC265" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L266" class="blob-num js-line-number" data-line-number="266"></td>
        <td id="LC266" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L267" class="blob-num js-line-number" data-line-number="267"></td>
        <td id="LC267" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>################</span></td>
      </tr>
      <tr>
        <td id="L268" class="blob-num js-line-number" data-line-number="268"></td>
        <td id="LC268" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> TARGET: p</span></td>
      </tr>
      <tr>
        <td id="L269" class="blob-num js-line-number" data-line-number="269"></td>
        <td id="LC269" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>################</span></td>
      </tr>
      <tr>
        <td id="L270" class="blob-num js-line-number" data-line-number="270"></td>
        <td id="LC270" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L271" class="blob-num js-line-number" data-line-number="271"></td>
        <td id="LC271" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m2_pre</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data_train</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L272" class="blob-num js-line-number" data-line-number="272"></td>
        <td id="LC272" class="blob-code blob-code-inner js-file-line">  model(<span class="pl-v">auto_arima</span> <span class="pl-k">=</span> ARIMA(<span class="pl-smi">p</span> <span class="pl-k">~</span>  PDQ(<span class="pl-c1">0</span>,<span class="pl-c1">0</span>,<span class="pl-c1">0</span>)))</td>
      </tr>
      <tr>
        <td id="L273" class="blob-num js-line-number" data-line-number="273"></td>
        <td id="LC273" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">p2_auto</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">m2_pre</span><span class="pl-k">$</span><span class="pl-smi">auto_arima</span>[[<span class="pl-c1">1</span>]]<span class="pl-k">$</span><span class="pl-smi">fit</span><span class="pl-k">$</span><span class="pl-smi">spec</span><span class="pl-k">$</span><span class="pl-smi">p</span></td>
      </tr>
      <tr>
        <td id="L274" class="blob-num js-line-number" data-line-number="274"></td>
        <td id="LC274" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">q2_auto</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">m2_pre</span><span class="pl-k">$</span><span class="pl-smi">auto_arima</span>[[<span class="pl-c1">1</span>]]<span class="pl-k">$</span><span class="pl-smi">fit</span><span class="pl-k">$</span><span class="pl-smi">spec</span><span class="pl-k">$</span><span class="pl-smi">q</span></td>
      </tr>
      <tr>
        <td id="L275" class="blob-num js-line-number" data-line-number="275"></td>
        <td id="LC275" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">d2_auto</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">m2_pre</span><span class="pl-k">$</span><span class="pl-smi">auto_arima</span>[[<span class="pl-c1">1</span>]]<span class="pl-k">$</span><span class="pl-smi">fit</span><span class="pl-k">$</span><span class="pl-smi">spec</span><span class="pl-k">$</span><span class="pl-smi">d</span></td>
      </tr>
      <tr>
        <td id="L276" class="blob-num js-line-number" data-line-number="276"></td>
        <td id="LC276" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L277" class="blob-num js-line-number" data-line-number="277"></td>
        <td id="LC277" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m3_pre</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data_train</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L278" class="blob-num js-line-number" data-line-number="278"></td>
        <td id="LC278" class="blob-code blob-code-inner js-file-line">  model(<span class="pl-v">auto_arima</span> <span class="pl-k">=</span> ARIMA(<span class="pl-smi">p</span> <span class="pl-k">~</span> season()<span class="pl-k">+</span>  PDQ(<span class="pl-c1">0</span>,<span class="pl-c1">0</span>,<span class="pl-c1">0</span>)))</td>
      </tr>
      <tr>
        <td id="L279" class="blob-num js-line-number" data-line-number="279"></td>
        <td id="LC279" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">p3_auto</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">m3_pre</span><span class="pl-k">$</span><span class="pl-smi">auto_arima</span>[[<span class="pl-c1">1</span>]]<span class="pl-k">$</span><span class="pl-smi">fit</span><span class="pl-k">$</span><span class="pl-smi">spec</span><span class="pl-k">$</span><span class="pl-smi">p</span></td>
      </tr>
      <tr>
        <td id="L280" class="blob-num js-line-number" data-line-number="280"></td>
        <td id="LC280" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">q3_auto</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">m3_pre</span><span class="pl-k">$</span><span class="pl-smi">auto_arima</span>[[<span class="pl-c1">1</span>]]<span class="pl-k">$</span><span class="pl-smi">fit</span><span class="pl-k">$</span><span class="pl-smi">spec</span><span class="pl-k">$</span><span class="pl-smi">q</span></td>
      </tr>
      <tr>
        <td id="L281" class="blob-num js-line-number" data-line-number="281"></td>
        <td id="LC281" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">d3_auto</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">m3_pre</span><span class="pl-k">$</span><span class="pl-smi">auto_arima</span>[[<span class="pl-c1">1</span>]]<span class="pl-k">$</span><span class="pl-smi">fit</span><span class="pl-k">$</span><span class="pl-smi">spec</span><span class="pl-k">$</span><span class="pl-smi">d</span></td>
      </tr>
      <tr>
        <td id="L282" class="blob-num js-line-number" data-line-number="282"></td>
        <td id="LC282" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L283" class="blob-num js-line-number" data-line-number="283"></td>
        <td id="LC283" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m4_pre</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data_train</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L284" class="blob-num js-line-number" data-line-number="284"></td>
        <td id="LC284" class="blob-code blob-code-inner js-file-line">  model(<span class="pl-v">auto_arima</span> <span class="pl-k">=</span> ARIMA(<span class="pl-smi">p</span> <span class="pl-k">~</span> season() <span class="pl-k">+</span> trend()<span class="pl-k">+</span> PDQ(<span class="pl-c1">0</span>,<span class="pl-c1">0</span>,<span class="pl-c1">0</span>)))</td>
      </tr>
      <tr>
        <td id="L285" class="blob-num js-line-number" data-line-number="285"></td>
        <td id="LC285" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">p4_auto</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">m4_pre</span><span class="pl-k">$</span><span class="pl-smi">auto_arima</span>[[<span class="pl-c1">1</span>]]<span class="pl-k">$</span><span class="pl-smi">fit</span><span class="pl-k">$</span><span class="pl-smi">spec</span><span class="pl-k">$</span><span class="pl-smi">p</span></td>
      </tr>
      <tr>
        <td id="L286" class="blob-num js-line-number" data-line-number="286"></td>
        <td id="LC286" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">q4_auto</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">m4_pre</span><span class="pl-k">$</span><span class="pl-smi">auto_arima</span>[[<span class="pl-c1">1</span>]]<span class="pl-k">$</span><span class="pl-smi">fit</span><span class="pl-k">$</span><span class="pl-smi">spec</span><span class="pl-k">$</span><span class="pl-smi">q</span></td>
      </tr>
      <tr>
        <td id="L287" class="blob-num js-line-number" data-line-number="287"></td>
        <td id="LC287" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">d4_auto</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">m4_pre</span><span class="pl-k">$</span><span class="pl-smi">auto_arima</span>[[<span class="pl-c1">1</span>]]<span class="pl-k">$</span><span class="pl-smi">fit</span><span class="pl-k">$</span><span class="pl-smi">spec</span><span class="pl-k">$</span><span class="pl-smi">d</span></td>
      </tr>
      <tr>
        <td id="L288" class="blob-num js-line-number" data-line-number="288"></td>
        <td id="LC288" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L289" class="blob-num js-line-number" data-line-number="289"></td>
        <td id="LC289" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L290" class="blob-num js-line-number" data-line-number="290"></td>
        <td id="LC290" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m1_p_formula</span> <span class="pl-k">&lt;-</span> <span class="pl-s"><span class="pl-pds">&quot;</span>p ~ season() + trend()<span class="pl-pds">&quot;</span></span></td>
      </tr>
      <tr>
        <td id="L291" class="blob-num js-line-number" data-line-number="291"></td>
        <td id="LC291" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m2_p_formula</span> <span class="pl-k">&lt;-</span> paste0(<span class="pl-s"><span class="pl-pds">&quot;</span>p ~  pdq(<span class="pl-pds">&quot;</span></span>,paste(<span class="pl-smi">p2_auto</span>,<span class="pl-smi">d2_auto</span>,<span class="pl-smi">q2_auto</span>, <span class="pl-v">sep</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span>,<span class="pl-pds">&quot;</span></span>),<span class="pl-s"><span class="pl-pds">&quot;</span>) + PDQ(0,0,0)<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L292" class="blob-num js-line-number" data-line-number="292"></td>
        <td id="LC292" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m3_p_formula</span> <span class="pl-k">&lt;-</span> paste0(<span class="pl-s"><span class="pl-pds">&quot;</span>p ~  pdq(<span class="pl-pds">&quot;</span></span>,paste(<span class="pl-smi">p3_auto</span>,<span class="pl-smi">d3_auto</span>,<span class="pl-smi">q3_auto</span>, <span class="pl-v">sep</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span>,<span class="pl-pds">&quot;</span></span>),<span class="pl-s"><span class="pl-pds">&quot;</span>) + PDQ(0,0,0) + season()<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L293" class="blob-num js-line-number" data-line-number="293"></td>
        <td id="LC293" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m4_p_formula</span> <span class="pl-k">&lt;-</span> paste0(<span class="pl-s"><span class="pl-pds">&quot;</span>p ~  pdq(<span class="pl-pds">&quot;</span></span>,paste(<span class="pl-smi">p4_auto</span>,<span class="pl-smi">d4_auto</span>,<span class="pl-smi">q4_auto</span>, <span class="pl-v">sep</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span>,<span class="pl-pds">&quot;</span></span>),<span class="pl-s"><span class="pl-pds">&quot;</span>) + PDQ(0,0,0) + season() + trend()<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L294" class="blob-num js-line-number" data-line-number="294"></td>
        <td id="LC294" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L295" class="blob-num js-line-number" data-line-number="295"></td>
        <td id="LC295" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m1_p</span> <span class="pl-k">&lt;-</span> TSLM(as.formula(<span class="pl-smi">m1_p_formula</span>))</td>
      </tr>
      <tr>
        <td id="L296" class="blob-num js-line-number" data-line-number="296"></td>
        <td id="LC296" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m2_p</span> <span class="pl-k">&lt;-</span>  ARIMA(as.formula(<span class="pl-smi">m2_p_formula</span>))</td>
      </tr>
      <tr>
        <td id="L297" class="blob-num js-line-number" data-line-number="297"></td>
        <td id="LC297" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m3_p</span> <span class="pl-k">&lt;-</span>  ARIMA(as.formula(<span class="pl-smi">m3_p_formula</span>))</td>
      </tr>
      <tr>
        <td id="L298" class="blob-num js-line-number" data-line-number="298"></td>
        <td id="LC298" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m4_p</span> <span class="pl-k">&lt;-</span>  ARIMA(as.formula(<span class="pl-smi">m4_p_formula</span>))</td>
      </tr>
      <tr>
        <td id="L299" class="blob-num js-line-number" data-line-number="299"></td>
        <td id="LC299" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L300" class="blob-num js-line-number" data-line-number="300"></td>
        <td id="LC300" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">models_p</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data_tr</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L301" class="blob-num js-line-number" data-line-number="301"></td>
        <td id="LC301" class="blob-code blob-code-inner js-file-line">  model(<span class="pl-v">m1_p</span> <span class="pl-k">=</span> <span class="pl-smi">m1_p</span>,</td>
      </tr>
      <tr>
        <td id="L302" class="blob-num js-line-number" data-line-number="302"></td>
        <td id="LC302" class="blob-code blob-code-inner js-file-line">        <span class="pl-v">m2_p</span> <span class="pl-k">=</span> <span class="pl-smi">m2_p</span>,</td>
      </tr>
      <tr>
        <td id="L303" class="blob-num js-line-number" data-line-number="303"></td>
        <td id="LC303" class="blob-code blob-code-inner js-file-line">        <span class="pl-v">m3_p</span> <span class="pl-k">=</span> <span class="pl-smi">m3_p</span>,</td>
      </tr>
      <tr>
        <td id="L304" class="blob-num js-line-number" data-line-number="304"></td>
        <td id="LC304" class="blob-code blob-code-inner js-file-line">        <span class="pl-v">m4_p</span> <span class="pl-k">=</span> <span class="pl-smi">m4_p</span></td>
      </tr>
      <tr>
        <td id="L305" class="blob-num js-line-number" data-line-number="305"></td>
        <td id="LC305" class="blob-code blob-code-inner js-file-line">        )</td>
      </tr>
      <tr>
        <td id="L306" class="blob-num js-line-number" data-line-number="306"></td>
        <td id="LC306" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L307" class="blob-num js-line-number" data-line-number="307"></td>
        <td id="LC307" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">rmse_train_p</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">models_p</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L308" class="blob-num js-line-number" data-line-number="308"></td>
        <td id="LC308" class="blob-code blob-code-inner js-file-line">  get_RMSE_from_model()</td>
      </tr>
      <tr>
        <td id="L309" class="blob-num js-line-number" data-line-number="309"></td>
        <td id="LC309" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L310" class="blob-num js-line-number" data-line-number="310"></td>
        <td id="LC310" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">forecast_p</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">models_p</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L311" class="blob-num js-line-number" data-line-number="311"></td>
        <td id="LC311" class="blob-code blob-code-inner js-file-line">  forecast(<span class="pl-v">h</span><span class="pl-k">=</span><span class="pl-c1">12</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L312" class="blob-num js-line-number" data-line-number="312"></td>
        <td id="LC312" class="blob-code blob-code-inner js-file-line">  as_tsibble() %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L313" class="blob-num js-line-number" data-line-number="313"></td>
        <td id="LC313" class="blob-code blob-code-inner js-file-line">  <span class="pl-e">dplyr</span><span class="pl-k">::</span>rename(<span class="pl-v">p_pred</span> <span class="pl-k">=</span> <span class="pl-smi">p</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L314" class="blob-num js-line-number" data-line-number="314"></td>
        <td id="LC314" class="blob-code blob-code-inner js-file-line">  left_join(<span class="pl-smi">data</span>[,c(<span class="pl-s"><span class="pl-pds">&quot;</span>date<span class="pl-pds">&quot;</span></span>,<span class="pl-s"><span class="pl-pds">&quot;</span>p<span class="pl-pds">&quot;</span></span>)]) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L315" class="blob-num js-line-number" data-line-number="315"></td>
        <td id="LC315" class="blob-code blob-code-inner js-file-line">  group_by(<span class="pl-smi">.id</span>, <span class="pl-smi">.model</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L316" class="blob-num js-line-number" data-line-number="316"></td>
        <td id="LC316" class="blob-code blob-code-inner js-file-line">  mutate(<span class="pl-v">e</span> <span class="pl-k">=</span> <span class="pl-smi">p</span> <span class="pl-k">-</span> <span class="pl-smi">p_pred</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L317" class="blob-num js-line-number" data-line-number="317"></td>
        <td id="LC317" class="blob-code blob-code-inner js-file-line">  ungroup()</td>
      </tr>
      <tr>
        <td id="L318" class="blob-num js-line-number" data-line-number="318"></td>
        <td id="LC318" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L319" class="blob-num js-line-number" data-line-number="319"></td>
        <td id="LC319" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Compute MSE for folds</span></td>
      </tr>
      <tr>
        <td id="L320" class="blob-num js-line-number" data-line-number="320"></td>
        <td id="LC320" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">summary_p</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">forecast_p</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L321" class="blob-num js-line-number" data-line-number="321"></td>
        <td id="LC321" class="blob-code blob-code-inner js-file-line">  get_MSE_from_forecast()</td>
      </tr>
      <tr>
        <td id="L322" class="blob-num js-line-number" data-line-number="322"></td>
        <td id="LC322" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L323" class="blob-num js-line-number" data-line-number="323"></td>
        <td id="LC323" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">summary_p</span></td>
      </tr>
      <tr>
        <td id="L324" class="blob-num js-line-number" data-line-number="324"></td>
        <td id="LC324" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L325" class="blob-num js-line-number" data-line-number="325"></td>
        <td id="LC325" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>################</span></td>
      </tr>
      <tr>
        <td id="L326" class="blob-num js-line-number" data-line-number="326"></td>
        <td id="LC326" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> TARGET: dp</span></td>
      </tr>
      <tr>
        <td id="L327" class="blob-num js-line-number" data-line-number="327"></td>
        <td id="LC327" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>################</span></td>
      </tr>
      <tr>
        <td id="L328" class="blob-num js-line-number" data-line-number="328"></td>
        <td id="LC328" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L329" class="blob-num js-line-number" data-line-number="329"></td>
        <td id="LC329" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L330" class="blob-num js-line-number" data-line-number="330"></td>
        <td id="LC330" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m12_pre</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data_train</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L331" class="blob-num js-line-number" data-line-number="331"></td>
        <td id="LC331" class="blob-code blob-code-inner js-file-line">  model(<span class="pl-v">auto_arima</span> <span class="pl-k">=</span> ARIMA(<span class="pl-smi">dp</span> <span class="pl-k">~</span> season()<span class="pl-k">+</span>  PDQ(<span class="pl-c1">0</span>,<span class="pl-c1">0</span>,<span class="pl-c1">0</span>)))</td>
      </tr>
      <tr>
        <td id="L332" class="blob-num js-line-number" data-line-number="332"></td>
        <td id="LC332" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">p12_auto</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">m12_pre</span><span class="pl-k">$</span><span class="pl-smi">auto_arima</span>[[<span class="pl-c1">1</span>]]<span class="pl-k">$</span><span class="pl-smi">fit</span><span class="pl-k">$</span><span class="pl-smi">spec</span><span class="pl-k">$</span><span class="pl-smi">p</span></td>
      </tr>
      <tr>
        <td id="L333" class="blob-num js-line-number" data-line-number="333"></td>
        <td id="LC333" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">q12_auto</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">m12_pre</span><span class="pl-k">$</span><span class="pl-smi">auto_arima</span>[[<span class="pl-c1">1</span>]]<span class="pl-k">$</span><span class="pl-smi">fit</span><span class="pl-k">$</span><span class="pl-smi">spec</span><span class="pl-k">$</span><span class="pl-smi">q</span></td>
      </tr>
      <tr>
        <td id="L334" class="blob-num js-line-number" data-line-number="334"></td>
        <td id="LC334" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">d12_auto</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">m12_pre</span><span class="pl-k">$</span><span class="pl-smi">auto_arima</span>[[<span class="pl-c1">1</span>]]<span class="pl-k">$</span><span class="pl-smi">fit</span><span class="pl-k">$</span><span class="pl-smi">spec</span><span class="pl-k">$</span><span class="pl-smi">d</span></td>
      </tr>
      <tr>
        <td id="L335" class="blob-num js-line-number" data-line-number="335"></td>
        <td id="LC335" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L336" class="blob-num js-line-number" data-line-number="336"></td>
        <td id="LC336" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Cross-validate models predicting first differences</span></td>
      </tr>
      <tr>
        <td id="L337" class="blob-num js-line-number" data-line-number="337"></td>
        <td id="LC337" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m11_dp_formula</span> <span class="pl-k">&lt;-</span> <span class="pl-s"><span class="pl-pds">&quot;</span>dp ~ season() + trend()<span class="pl-pds">&quot;</span></span></td>
      </tr>
      <tr>
        <td id="L338" class="blob-num js-line-number" data-line-number="338"></td>
        <td id="LC338" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m12_dp_formula</span> <span class="pl-k">&lt;-</span> paste0(<span class="pl-s"><span class="pl-pds">&quot;</span>dp ~ season()+ pdq(<span class="pl-pds">&quot;</span></span>,paste(<span class="pl-smi">p12_auto</span>,<span class="pl-smi">d12_auto</span>,<span class="pl-smi">q12_auto</span>, <span class="pl-v">sep</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span>,<span class="pl-pds">&quot;</span></span>),<span class="pl-s"><span class="pl-pds">&quot;</span>) + PDQ(0,0,0)<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L339" class="blob-num js-line-number" data-line-number="339"></td>
        <td id="LC339" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L340" class="blob-num js-line-number" data-line-number="340"></td>
        <td id="LC340" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m11_dp</span> <span class="pl-k">&lt;-</span>  TSLM(as.formula(<span class="pl-smi">m11_dp_formula</span>))</td>
      </tr>
      <tr>
        <td id="L341" class="blob-num js-line-number" data-line-number="341"></td>
        <td id="LC341" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m12_dp</span> <span class="pl-k">&lt;-</span>  ARIMA(as.formula(<span class="pl-smi">m12_dp_formula</span>))</td>
      </tr>
      <tr>
        <td id="L342" class="blob-num js-line-number" data-line-number="342"></td>
        <td id="LC342" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L343" class="blob-num js-line-number" data-line-number="343"></td>
        <td id="LC343" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">models_dp</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data_tr</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L344" class="blob-num js-line-number" data-line-number="344"></td>
        <td id="LC344" class="blob-code blob-code-inner js-file-line">  model(<span class="pl-v">m11_dp</span> <span class="pl-k">=</span> <span class="pl-smi">m11_dp</span>,</td>
      </tr>
      <tr>
        <td id="L345" class="blob-num js-line-number" data-line-number="345"></td>
        <td id="LC345" class="blob-code blob-code-inner js-file-line">        <span class="pl-v">m12_dp</span> <span class="pl-k">=</span> <span class="pl-smi">m12_dp</span>)</td>
      </tr>
      <tr>
        <td id="L346" class="blob-num js-line-number" data-line-number="346"></td>
        <td id="LC346" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L347" class="blob-num js-line-number" data-line-number="347"></td>
        <td id="LC347" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">rmse_train_dp</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">models_dp</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L348" class="blob-num js-line-number" data-line-number="348"></td>
        <td id="LC348" class="blob-code blob-code-inner js-file-line">  get_RMSE_from_model()</td>
      </tr>
      <tr>
        <td id="L349" class="blob-num js-line-number" data-line-number="349"></td>
        <td id="LC349" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L350" class="blob-num js-line-number" data-line-number="350"></td>
        <td id="LC350" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">forecast_dp</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">models_dp</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L351" class="blob-num js-line-number" data-line-number="351"></td>
        <td id="LC351" class="blob-code blob-code-inner js-file-line">  forecast(<span class="pl-v">h</span><span class="pl-k">=</span><span class="pl-c1">12</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L352" class="blob-num js-line-number" data-line-number="352"></td>
        <td id="LC352" class="blob-code blob-code-inner js-file-line">  as_tsibble() %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L353" class="blob-num js-line-number" data-line-number="353"></td>
        <td id="LC353" class="blob-code blob-code-inner js-file-line">  <span class="pl-e">dplyr</span><span class="pl-k">::</span>rename(<span class="pl-v">dp_pred</span> <span class="pl-k">=</span> <span class="pl-smi">dp</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L354" class="blob-num js-line-number" data-line-number="354"></td>
        <td id="LC354" class="blob-code blob-code-inner js-file-line">  left_join(<span class="pl-smi">data</span>[,c(<span class="pl-s"><span class="pl-pds">&quot;</span>date<span class="pl-pds">&quot;</span></span>,<span class="pl-s"><span class="pl-pds">&quot;</span>p<span class="pl-pds">&quot;</span></span>,<span class="pl-s"><span class="pl-pds">&quot;</span>p_lag<span class="pl-pds">&quot;</span></span>)]) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L355" class="blob-num js-line-number" data-line-number="355"></td>
        <td id="LC355" class="blob-code blob-code-inner js-file-line">  group_by(<span class="pl-smi">.id</span>, <span class="pl-smi">.model</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L356" class="blob-num js-line-number" data-line-number="356"></td>
        <td id="LC356" class="blob-code blob-code-inner js-file-line">  mutate(<span class="pl-v">p_pred</span> <span class="pl-k">=</span> cumsum(<span class="pl-smi">dp_pred</span>) <span class="pl-k">+</span> <span class="pl-smi">p_lag</span>[<span class="pl-c1">1</span>]) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L357" class="blob-num js-line-number" data-line-number="357"></td>
        <td id="LC357" class="blob-code blob-code-inner js-file-line">  mutate(<span class="pl-v">e</span> <span class="pl-k">=</span> <span class="pl-smi">p</span> <span class="pl-k">-</span> <span class="pl-smi">p_pred</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L358" class="blob-num js-line-number" data-line-number="358"></td>
        <td id="LC358" class="blob-code blob-code-inner js-file-line">  ungroup()</td>
      </tr>
      <tr>
        <td id="L359" class="blob-num js-line-number" data-line-number="359"></td>
        <td id="LC359" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L360" class="blob-num js-line-number" data-line-number="360"></td>
        <td id="LC360" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Compute MSE for folds</span></td>
      </tr>
      <tr>
        <td id="L361" class="blob-num js-line-number" data-line-number="361"></td>
        <td id="LC361" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">summary_dp</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">forecast_dp</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L362" class="blob-num js-line-number" data-line-number="362"></td>
        <td id="LC362" class="blob-code blob-code-inner js-file-line">  get_MSE_from_forecast()</td>
      </tr>
      <tr>
        <td id="L363" class="blob-num js-line-number" data-line-number="363"></td>
        <td id="LC363" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">summary_dp</span></td>
      </tr>
      <tr>
        <td id="L364" class="blob-num js-line-number" data-line-number="364"></td>
        <td id="LC364" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L365" class="blob-num js-line-number" data-line-number="365"></td>
        <td id="LC365" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>################</span></td>
      </tr>
      <tr>
        <td id="L366" class="blob-num js-line-number" data-line-number="366"></td>
        <td id="LC366" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> TARGET: lnp</span></td>
      </tr>
      <tr>
        <td id="L367" class="blob-num js-line-number" data-line-number="367"></td>
        <td id="LC367" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>################</span></td>
      </tr>
      <tr>
        <td id="L368" class="blob-num js-line-number" data-line-number="368"></td>
        <td id="LC368" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L369" class="blob-num js-line-number" data-line-number="369"></td>
        <td id="LC369" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m21_pre</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data_train</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L370" class="blob-num js-line-number" data-line-number="370"></td>
        <td id="LC370" class="blob-code blob-code-inner js-file-line">  model(<span class="pl-v">auto_arima</span> <span class="pl-k">=</span> ARIMA(<span class="pl-smi">lnp</span> <span class="pl-k">~</span>  season()<span class="pl-k">+</span> PDQ(<span class="pl-c1">0</span>,<span class="pl-c1">0</span>,<span class="pl-c1">0</span>)))</td>
      </tr>
      <tr>
        <td id="L371" class="blob-num js-line-number" data-line-number="371"></td>
        <td id="LC371" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">p21_auto</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">m21_pre</span><span class="pl-k">$</span><span class="pl-smi">auto_arima</span>[[<span class="pl-c1">1</span>]]<span class="pl-k">$</span><span class="pl-smi">fit</span><span class="pl-k">$</span><span class="pl-smi">spec</span><span class="pl-k">$</span><span class="pl-smi">p</span></td>
      </tr>
      <tr>
        <td id="L372" class="blob-num js-line-number" data-line-number="372"></td>
        <td id="LC372" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">q21_auto</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">m21_pre</span><span class="pl-k">$</span><span class="pl-smi">auto_arima</span>[[<span class="pl-c1">1</span>]]<span class="pl-k">$</span><span class="pl-smi">fit</span><span class="pl-k">$</span><span class="pl-smi">spec</span><span class="pl-k">$</span><span class="pl-smi">q</span></td>
      </tr>
      <tr>
        <td id="L373" class="blob-num js-line-number" data-line-number="373"></td>
        <td id="LC373" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">d21_auto</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">m21_pre</span><span class="pl-k">$</span><span class="pl-smi">auto_arima</span>[[<span class="pl-c1">1</span>]]<span class="pl-k">$</span><span class="pl-smi">fit</span><span class="pl-k">$</span><span class="pl-smi">spec</span><span class="pl-k">$</span><span class="pl-smi">d</span></td>
      </tr>
      <tr>
        <td id="L374" class="blob-num js-line-number" data-line-number="374"></td>
        <td id="LC374" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L375" class="blob-num js-line-number" data-line-number="375"></td>
        <td id="LC375" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m21_lnp_formula</span> <span class="pl-k">&lt;-</span> paste0(<span class="pl-s"><span class="pl-pds">&quot;</span>lnp ~ season()+ pdq(<span class="pl-pds">&quot;</span></span>,paste(<span class="pl-smi">p21_auto</span>,<span class="pl-smi">d21_auto</span>,<span class="pl-smi">q21_auto</span>, <span class="pl-v">sep</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span>,<span class="pl-pds">&quot;</span></span>),<span class="pl-s"><span class="pl-pds">&quot;</span>) + PDQ(0,0,0)<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L376" class="blob-num js-line-number" data-line-number="376"></td>
        <td id="LC376" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">m21_lnp</span> <span class="pl-k">&lt;-</span>  ARIMA(as.formula(<span class="pl-smi">m21_lnp_formula</span>))</td>
      </tr>
      <tr>
        <td id="L377" class="blob-num js-line-number" data-line-number="377"></td>
        <td id="LC377" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L378" class="blob-num js-line-number" data-line-number="378"></td>
        <td id="LC378" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">models_lnp</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data_tr</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L379" class="blob-num js-line-number" data-line-number="379"></td>
        <td id="LC379" class="blob-code blob-code-inner js-file-line">  model(<span class="pl-v">m21_lnp</span> <span class="pl-k">=</span> <span class="pl-smi">m21_lnp</span>)</td>
      </tr>
      <tr>
        <td id="L380" class="blob-num js-line-number" data-line-number="380"></td>
        <td id="LC380" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L381" class="blob-num js-line-number" data-line-number="381"></td>
        <td id="LC381" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">rmse_train_lnp</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">models_lnp</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L382" class="blob-num js-line-number" data-line-number="382"></td>
        <td id="LC382" class="blob-code blob-code-inner js-file-line">  get_RMSE_from_model()</td>
      </tr>
      <tr>
        <td id="L383" class="blob-num js-line-number" data-line-number="383"></td>
        <td id="LC383" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L384" class="blob-num js-line-number" data-line-number="384"></td>
        <td id="LC384" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">forecast_lnp</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">models_lnp</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L385" class="blob-num js-line-number" data-line-number="385"></td>
        <td id="LC385" class="blob-code blob-code-inner js-file-line">  forecast(<span class="pl-v">h</span><span class="pl-k">=</span><span class="pl-c1">12</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L386" class="blob-num js-line-number" data-line-number="386"></td>
        <td id="LC386" class="blob-code blob-code-inner js-file-line">  as_tsibble() %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L387" class="blob-num js-line-number" data-line-number="387"></td>
        <td id="LC387" class="blob-code blob-code-inner js-file-line">  <span class="pl-e">dplyr</span><span class="pl-k">::</span>rename(<span class="pl-v">lnp_pred</span> <span class="pl-k">=</span> <span class="pl-smi">lnp</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L388" class="blob-num js-line-number" data-line-number="388"></td>
        <td id="LC388" class="blob-code blob-code-inner js-file-line">  left_join(<span class="pl-smi">data</span>[,c(<span class="pl-s"><span class="pl-pds">&quot;</span>date<span class="pl-pds">&quot;</span></span>,<span class="pl-s"><span class="pl-pds">&quot;</span>p<span class="pl-pds">&quot;</span></span>)]) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L389" class="blob-num js-line-number" data-line-number="389"></td>
        <td id="LC389" class="blob-code blob-code-inner js-file-line">  left_join(<span class="pl-smi">rmse_train_lnp</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L390" class="blob-num js-line-number" data-line-number="390"></td>
        <td id="LC390" class="blob-code blob-code-inner js-file-line">  group_by(<span class="pl-smi">.id</span>, <span class="pl-smi">.model</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L391" class="blob-num js-line-number" data-line-number="391"></td>
        <td id="LC391" class="blob-code blob-code-inner js-file-line">  mutate(<span class="pl-v">p_pred</span> <span class="pl-k">=</span> exp(<span class="pl-smi">lnp_pred</span>)<span class="pl-k">*</span>exp((<span class="pl-smi">RMSE</span><span class="pl-k">**</span><span class="pl-c1">2</span>)<span class="pl-k">/</span><span class="pl-c1">2</span>) ) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L392" class="blob-num js-line-number" data-line-number="392"></td>
        <td id="LC392" class="blob-code blob-code-inner js-file-line">  mutate(<span class="pl-v">e</span> <span class="pl-k">=</span> <span class="pl-smi">p</span> <span class="pl-k">-</span> <span class="pl-smi">p_pred</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L393" class="blob-num js-line-number" data-line-number="393"></td>
        <td id="LC393" class="blob-code blob-code-inner js-file-line">  ungroup()</td>
      </tr>
      <tr>
        <td id="L394" class="blob-num js-line-number" data-line-number="394"></td>
        <td id="LC394" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L395" class="blob-num js-line-number" data-line-number="395"></td>
        <td id="LC395" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Compute MSE for folds</span></td>
      </tr>
      <tr>
        <td id="L396" class="blob-num js-line-number" data-line-number="396"></td>
        <td id="LC396" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">summary_lnp</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">forecast_lnp</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L397" class="blob-num js-line-number" data-line-number="397"></td>
        <td id="LC397" class="blob-code blob-code-inner js-file-line">  get_MSE_from_forecast()</td>
      </tr>
      <tr>
        <td id="L398" class="blob-num js-line-number" data-line-number="398"></td>
        <td id="LC398" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">summary_lnp</span></td>
      </tr>
      <tr>
        <td id="L399" class="blob-num js-line-number" data-line-number="399"></td>
        <td id="LC399" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L400" class="blob-num js-line-number" data-line-number="400"></td>
        <td id="LC400" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L401" class="blob-num js-line-number" data-line-number="401"></td>
        <td id="LC401" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> VAR</span></td>
      </tr>
      <tr>
        <td id="L402" class="blob-num js-line-number" data-line-number="402"></td>
        <td id="LC402" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">var2_formula</span> <span class="pl-k">&lt;-</span> <span class="pl-s"><span class="pl-pds">&quot;</span>vars(dp, du, dlnemp) ~ AR(1)<span class="pl-pds">&quot;</span></span></td>
      </tr>
      <tr>
        <td id="L403" class="blob-num js-line-number" data-line-number="403"></td>
        <td id="LC403" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">var2</span> <span class="pl-k">&lt;-</span> VAR(as.formula(<span class="pl-smi">var2_formula</span>))</td>
      </tr>
      <tr>
        <td id="L404" class="blob-num js-line-number" data-line-number="404"></td>
        <td id="LC404" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L405" class="blob-num js-line-number" data-line-number="405"></td>
        <td id="LC405" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">var21</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data_tr</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L406" class="blob-num js-line-number" data-line-number="406"></td>
        <td id="LC406" class="blob-code blob-code-inner js-file-line">  filter(<span class="pl-k">!</span>is.na(<span class="pl-smi">dp</span>)) %<span class="pl-k">&gt;</span>% <span class="pl-c"><span class="pl-c">#</span> need to exclude first row</span></td>
      </tr>
      <tr>
        <td id="L407" class="blob-num js-line-number" data-line-number="407"></td>
        <td id="LC407" class="blob-code blob-code-inner js-file-line">  model(<span class="pl-v">var2</span> <span class="pl-k">=</span> <span class="pl-smi">var2</span>)</td>
      </tr>
      <tr>
        <td id="L408" class="blob-num js-line-number" data-line-number="408"></td>
        <td id="LC408" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L409" class="blob-num js-line-number" data-line-number="409"></td>
        <td id="LC409" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">rmse_train_var2</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">var21</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L410" class="blob-num js-line-number" data-line-number="410"></td>
        <td id="LC410" class="blob-code blob-code-inner js-file-line">  get_RMSE_from_model(<span class="pl-v">resid_col_name</span> <span class="pl-k">=</span> <span class="pl-s"><span class="pl-pds">&quot;</span>dp<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L411" class="blob-num js-line-number" data-line-number="411"></td>
        <td id="LC411" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L412" class="blob-num js-line-number" data-line-number="412"></td>
        <td id="LC412" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L413" class="blob-num js-line-number" data-line-number="413"></td>
        <td id="LC413" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">forecast_var2</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">var21</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L414" class="blob-num js-line-number" data-line-number="414"></td>
        <td id="LC414" class="blob-code blob-code-inner js-file-line">  forecast(<span class="pl-v">h</span><span class="pl-k">=</span><span class="pl-c1">12</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L415" class="blob-num js-line-number" data-line-number="415"></td>
        <td id="LC415" class="blob-code blob-code-inner js-file-line">  as_tsibble() %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L416" class="blob-num js-line-number" data-line-number="416"></td>
        <td id="LC416" class="blob-code blob-code-inner js-file-line">  <span class="pl-e">dplyr</span><span class="pl-k">::</span>rename(<span class="pl-v">dp_pred</span> <span class="pl-k">=</span> <span class="pl-smi">dp</span>)  %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L417" class="blob-num js-line-number" data-line-number="417"></td>
        <td id="LC417" class="blob-code blob-code-inner js-file-line">  left_join(<span class="pl-smi">data</span>[,c(<span class="pl-s"><span class="pl-pds">&quot;</span>date<span class="pl-pds">&quot;</span></span>,<span class="pl-s"><span class="pl-pds">&quot;</span>p<span class="pl-pds">&quot;</span></span>,<span class="pl-s"><span class="pl-pds">&quot;</span>p_lag<span class="pl-pds">&quot;</span></span>)]) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L418" class="blob-num js-line-number" data-line-number="418"></td>
        <td id="LC418" class="blob-code blob-code-inner js-file-line">  group_by(<span class="pl-smi">.id</span>, <span class="pl-smi">.model</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L419" class="blob-num js-line-number" data-line-number="419"></td>
        <td id="LC419" class="blob-code blob-code-inner js-file-line">  mutate(<span class="pl-v">p_pred</span> <span class="pl-k">=</span> cumsum(<span class="pl-smi">dp_pred</span>) <span class="pl-k">+</span> <span class="pl-smi">p_lag</span>[<span class="pl-c1">1</span>]) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L420" class="blob-num js-line-number" data-line-number="420"></td>
        <td id="LC420" class="blob-code blob-code-inner js-file-line">  mutate(<span class="pl-v">e</span> <span class="pl-k">=</span> <span class="pl-smi">p</span> <span class="pl-k">-</span> <span class="pl-smi">p_pred</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L421" class="blob-num js-line-number" data-line-number="421"></td>
        <td id="LC421" class="blob-code blob-code-inner js-file-line">  ungroup()</td>
      </tr>
      <tr>
        <td id="L422" class="blob-num js-line-number" data-line-number="422"></td>
        <td id="LC422" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L423" class="blob-num js-line-number" data-line-number="423"></td>
        <td id="LC423" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Compute MSE for folds</span></td>
      </tr>
      <tr>
        <td id="L424" class="blob-num js-line-number" data-line-number="424"></td>
        <td id="LC424" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">summary_var2</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">forecast_var2</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L425" class="blob-num js-line-number" data-line-number="425"></td>
        <td id="LC425" class="blob-code blob-code-inner js-file-line">  get_MSE_from_forecast()</td>
      </tr>
      <tr>
        <td id="L426" class="blob-num js-line-number" data-line-number="426"></td>
        <td id="LC426" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">summary_var2</span></td>
      </tr>
      <tr>
        <td id="L427" class="blob-num js-line-number" data-line-number="427"></td>
        <td id="LC427" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L428" class="blob-num js-line-number" data-line-number="428"></td>
        <td id="LC428" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L429" class="blob-num js-line-number" data-line-number="429"></td>
        <td id="LC429" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>############################</span></td>
      </tr>
      <tr>
        <td id="L430" class="blob-num js-line-number" data-line-number="430"></td>
        <td id="LC430" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> COMPARISON</span></td>
      </tr>
      <tr>
        <td id="L431" class="blob-num js-line-number" data-line-number="431"></td>
        <td id="LC431" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>############################</span></td>
      </tr>
      <tr>
        <td id="L432" class="blob-num js-line-number" data-line-number="432"></td>
        <td id="LC432" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L433" class="blob-num js-line-number" data-line-number="433"></td>
        <td id="LC433" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">summary_folds</span> <span class="pl-k">&lt;-</span> bind_rows(<span class="pl-k">list</span>(<span class="pl-smi">summary_p</span>, <span class="pl-smi">summary_dp</span>, <span class="pl-smi">summary_lnp</span>, <span class="pl-smi">summary_var2</span>)) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L434" class="blob-num js-line-number" data-line-number="434"></td>
        <td id="LC434" class="blob-code blob-code-inner js-file-line">  spread(<span class="pl-smi">.id</span>, <span class="pl-smi">MSE</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L435" class="blob-num js-line-number" data-line-number="435"></td>
        <td id="LC435" class="blob-code blob-code-inner js-file-line">  as.data.frame()</td>
      </tr>
      <tr>
        <td id="L436" class="blob-num js-line-number" data-line-number="436"></td>
        <td id="LC436" class="blob-code blob-code-inner js-file-line">colnames(<span class="pl-smi">summary_folds</span>) <span class="pl-k">&lt;-</span> c(<span class="pl-s"><span class="pl-pds">&quot;</span>Model<span class="pl-pds">&quot;</span></span>, paste0(<span class="pl-s"><span class="pl-pds">&quot;</span>Fold <span class="pl-pds">&quot;</span></span>, colnames(<span class="pl-smi">summary_folds</span>)[<span class="pl-k">-</span><span class="pl-c1">1</span>]))</td>
      </tr>
      <tr>
        <td id="L437" class="blob-num js-line-number" data-line-number="437"></td>
        <td id="LC437" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L438" class="blob-num js-line-number" data-line-number="438"></td>
        <td id="LC438" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">summary_final</span> <span class="pl-k">&lt;-</span> bind_rows(<span class="pl-k">list</span>(<span class="pl-smi">summary_p</span>, <span class="pl-smi">summary_dp</span>, <span class="pl-smi">summary_lnp</span>, <span class="pl-smi">summary_var2</span>)) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L439" class="blob-num js-line-number" data-line-number="439"></td>
        <td id="LC439" class="blob-code blob-code-inner js-file-line">  group_by(<span class="pl-smi">.model</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L440" class="blob-num js-line-number" data-line-number="440"></td>
        <td id="LC440" class="blob-code blob-code-inner js-file-line">  <span class="pl-e">dplyr</span><span class="pl-k">::</span>summarise(<span class="pl-v">CV_RMSE</span> <span class="pl-k">=</span> sum(<span class="pl-smi">MSE</span>)<span class="pl-k">**</span><span class="pl-c1">0.5</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L441" class="blob-num js-line-number" data-line-number="441"></td>
        <td id="LC441" class="blob-code blob-code-inner js-file-line">  as.data.frame()</td>
      </tr>
      <tr>
        <td id="L442" class="blob-num js-line-number" data-line-number="442"></td>
        <td id="LC442" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L443" class="blob-num js-line-number" data-line-number="443"></td>
        <td id="LC443" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">model_formulas</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">summary_final</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L444" class="blob-num js-line-number" data-line-number="444"></td>
        <td id="LC444" class="blob-code blob-code-inner js-file-line">  <span class="pl-e">dplyr</span><span class="pl-k">::</span>pull(<span class="pl-smi">.model</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L445" class="blob-num js-line-number" data-line-number="445"></td>
        <td id="LC445" class="blob-code blob-code-inner js-file-line">  paste0(<span class="pl-s"><span class="pl-pds">&quot;</span>_formula<span class="pl-pds">&quot;</span></span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L446" class="blob-num js-line-number" data-line-number="446"></td>
        <td id="LC446" class="blob-code blob-code-inner js-file-line">  sapply(<span class="pl-v">FUN</span><span class="pl-k">=</span><span class="pl-smi">get</span>)</td>
      </tr>
      <tr>
        <td id="L447" class="blob-num js-line-number" data-line-number="447"></td>
        <td id="LC447" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L448" class="blob-num js-line-number" data-line-number="448"></td>
        <td id="LC448" class="blob-code blob-code-inner js-file-line">colnames(<span class="pl-smi">summary_final</span>) <span class="pl-k">&lt;-</span> c(<span class="pl-s"><span class="pl-pds">&quot;</span>Model<span class="pl-pds">&quot;</span></span>, <span class="pl-s"><span class="pl-pds">&quot;</span>CV RMSE<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L449" class="blob-num js-line-number" data-line-number="449"></td>
        <td id="LC449" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">summary_final</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">summary_final</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L450" class="blob-num js-line-number" data-line-number="450"></td>
        <td id="LC450" class="blob-code blob-code-inner js-file-line">  add_column(<span class="pl-s"><span class="pl-pds">&quot;</span>Model def<span class="pl-pds">&quot;</span></span> <span class="pl-k">=</span> <span class="pl-smi">model_formulas</span>, <span class="pl-v">.before</span> <span class="pl-k">=</span> <span class="pl-s"><span class="pl-pds">&quot;</span>CV RMSE<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L451" class="blob-num js-line-number" data-line-number="451"></td>
        <td id="LC451" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L452" class="blob-num js-line-number" data-line-number="452"></td>
        <td id="LC452" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">summary_final</span></td>
      </tr>
      <tr>
        <td id="L453" class="blob-num js-line-number" data-line-number="453"></td>
        <td id="LC453" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L454" class="blob-num js-line-number" data-line-number="454"></td>
        <td id="LC454" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>##########################x</span></td>
      </tr>
      <tr>
        <td id="L455" class="blob-num js-line-number" data-line-number="455"></td>
        <td id="LC455" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> predict for holdout</span></td>
      </tr>
      <tr>
        <td id="L456" class="blob-num js-line-number" data-line-number="456"></td>
        <td id="LC456" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>##########################x</span></td>
      </tr>
      <tr>
        <td id="L457" class="blob-num js-line-number" data-line-number="457"></td>
        <td id="LC457" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">conf_level</span> <span class="pl-k">&lt;-</span>  <span class="pl-c1">80</span></td>
      </tr>
      <tr>
        <td id="L458" class="blob-num js-line-number" data-line-number="458"></td>
        <td id="LC458" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L459" class="blob-num js-line-number" data-line-number="459"></td>
        <td id="LC459" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> best model when p is target</span></td>
      </tr>
      <tr>
        <td id="L460" class="blob-num js-line-number" data-line-number="460"></td>
        <td id="LC460" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> (need to adjust code if lnp is best)</span></td>
      </tr>
      <tr>
        <td id="L461" class="blob-num js-line-number" data-line-number="461"></td>
        <td id="LC461" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L462" class="blob-num js-line-number" data-line-number="462"></td>
        <td id="LC462" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">best_arima_model_p</span> <span class="pl-k">&lt;-</span> <span class="pl-s"><span class="pl-pds">&quot;</span>m4_p<span class="pl-pds">&quot;</span></span></td>
      </tr>
      <tr>
        <td id="L463" class="blob-num js-line-number" data-line-number="463"></td>
        <td id="LC463" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L464" class="blob-num js-line-number" data-line-number="464"></td>
        <td id="LC464" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> re-estimate best models on full train set</span></td>
      </tr>
      <tr>
        <td id="L465" class="blob-num js-line-number" data-line-number="465"></td>
        <td id="LC465" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">models_best_arima_p</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data_train</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L466" class="blob-num js-line-number" data-line-number="466"></td>
        <td id="LC466" class="blob-code blob-code-inner js-file-line">  model(<span class="pl-v">best</span> <span class="pl-k">=</span> get(<span class="pl-smi">best_arima_model_p</span>))</td>
      </tr>
      <tr>
        <td id="L467" class="blob-num js-line-number" data-line-number="467"></td>
        <td id="LC467" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L468" class="blob-num js-line-number" data-line-number="468"></td>
        <td id="LC468" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">rmse_train_best_p</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">models_best_arima_p</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L469" class="blob-num js-line-number" data-line-number="469"></td>
        <td id="LC469" class="blob-code blob-code-inner js-file-line">  get_RMSE_from_model(<span class="pl-v">groupby</span> <span class="pl-k">=</span> c(<span class="pl-s"><span class="pl-pds">&quot;</span>.model<span class="pl-pds">&quot;</span></span>))</td>
      </tr>
      <tr>
        <td id="L470" class="blob-num js-line-number" data-line-number="470"></td>
        <td id="LC470" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L471" class="blob-num js-line-number" data-line-number="471"></td>
        <td id="LC471" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">forecast_holdout_best_arima_p</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">models_best_arima_p</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L472" class="blob-num js-line-number" data-line-number="472"></td>
        <td id="LC472" class="blob-code blob-code-inner js-file-line">  forecast(<span class="pl-v">h</span><span class="pl-k">=</span><span class="pl-c1">12</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L473" class="blob-num js-line-number" data-line-number="473"></td>
        <td id="LC473" class="blob-code blob-code-inner js-file-line">  hilo(<span class="pl-v">level</span> <span class="pl-k">=</span> c(<span class="pl-smi">conf_level</span>)) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L474" class="blob-num js-line-number" data-line-number="474"></td>
        <td id="LC474" class="blob-code blob-code-inner js-file-line">  as_tsibble() %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L475" class="blob-num js-line-number" data-line-number="475"></td>
        <td id="LC475" class="blob-code blob-code-inner js-file-line">  rename(<span class="pl-v">p_pred</span> <span class="pl-k">=</span> <span class="pl-smi">p</span>)  %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L476" class="blob-num js-line-number" data-line-number="476"></td>
        <td id="LC476" class="blob-code blob-code-inner js-file-line">  left_join(<span class="pl-smi">data_holdout</span>[,c(<span class="pl-s"><span class="pl-pds">&quot;</span>date<span class="pl-pds">&quot;</span></span>,<span class="pl-s"><span class="pl-pds">&quot;</span>p<span class="pl-pds">&quot;</span></span>)]) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L477" class="blob-num js-line-number" data-line-number="477"></td>
        <td id="LC477" class="blob-code blob-code-inner js-file-line">  mutate(<span class="pl-v">e</span> <span class="pl-k">=</span> <span class="pl-smi">p</span> <span class="pl-k">-</span> <span class="pl-smi">p_pred</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L478" class="blob-num js-line-number" data-line-number="478"></td>
        <td id="LC478" class="blob-code blob-code-inner js-file-line">  ungroup()</td>
      </tr>
      <tr>
        <td id="L479" class="blob-num js-line-number" data-line-number="479"></td>
        <td id="LC479" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L480" class="blob-num js-line-number" data-line-number="480"></td>
        <td id="LC480" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">summary_holdout_best_arima_p</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">forecast_holdout_best_arima_p</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L481" class="blob-num js-line-number" data-line-number="481"></td>
        <td id="LC481" class="blob-code blob-code-inner js-file-line">  get_MSE_from_forecast(<span class="pl-v">groupby</span> <span class="pl-k">=</span> c(<span class="pl-s"><span class="pl-pds">&quot;</span>.model<span class="pl-pds">&quot;</span></span>))</td>
      </tr>
      <tr>
        <td id="L482" class="blob-num js-line-number" data-line-number="482"></td>
        <td id="LC482" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L483" class="blob-num js-line-number" data-line-number="483"></td>
        <td id="LC483" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> if var is better</span></td>
      </tr>
      <tr>
        <td id="L484" class="blob-num js-line-number" data-line-number="484"></td>
        <td id="LC484" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>  var</span></td>
      </tr>
      <tr>
        <td id="L485" class="blob-num js-line-number" data-line-number="485"></td>
        <td id="LC485" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L486" class="blob-num js-line-number" data-line-number="486"></td>
        <td id="LC486" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> var21 &lt;- data_train %&gt;%</span></td>
      </tr>
      <tr>
        <td id="L487" class="blob-num js-line-number" data-line-number="487"></td>
        <td id="LC487" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>   filter(!is.na(dp)) %&gt;% # need to exclude first row</span></td>
      </tr>
      <tr>
        <td id="L488" class="blob-num js-line-number" data-line-number="488"></td>
        <td id="LC488" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>   model(var2 = var2)</span></td>
      </tr>
      <tr>
        <td id="L489" class="blob-num js-line-number" data-line-number="489"></td>
        <td id="LC489" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span></span></td>
      </tr>
      <tr>
        <td id="L490" class="blob-num js-line-number" data-line-number="490"></td>
        <td id="LC490" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> rmse_train_var2 &lt;- var21 %&gt;%</span></td>
      </tr>
      <tr>
        <td id="L491" class="blob-num js-line-number" data-line-number="491"></td>
        <td id="LC491" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>   get_RMSE_from_model(resid_col_name = &quot;dp&quot;)</span></td>
      </tr>
      <tr>
        <td id="L492" class="blob-num js-line-number" data-line-number="492"></td>
        <td id="LC492" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span></span></td>
      </tr>
      <tr>
        <td id="L493" class="blob-num js-line-number" data-line-number="493"></td>
        <td id="LC493" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span></span></td>
      </tr>
      <tr>
        <td id="L494" class="blob-num js-line-number" data-line-number="494"></td>
        <td id="LC494" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> forecast_var2 &lt;- var21 %&gt;%</span></td>
      </tr>
      <tr>
        <td id="L495" class="blob-num js-line-number" data-line-number="495"></td>
        <td id="LC495" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>   forecast(h=12) %&gt;%</span></td>
      </tr>
      <tr>
        <td id="L496" class="blob-num js-line-number" data-line-number="496"></td>
        <td id="LC496" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>   as_tsibble() %&gt;%</span></td>
      </tr>
      <tr>
        <td id="L497" class="blob-num js-line-number" data-line-number="497"></td>
        <td id="LC497" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>   rename(dp_pred = dp)  %&gt;%</span></td>
      </tr>
      <tr>
        <td id="L498" class="blob-num js-line-number" data-line-number="498"></td>
        <td id="LC498" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>   left_join(data_holdout[,c(&quot;date&quot;,&quot;p&quot;,&quot;p_lag&quot;)]) %&gt;%</span></td>
      </tr>
      <tr>
        <td id="L499" class="blob-num js-line-number" data-line-number="499"></td>
        <td id="LC499" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>   group_by(.id, .model) %&gt;%</span></td>
      </tr>
      <tr>
        <td id="L500" class="blob-num js-line-number" data-line-number="500"></td>
        <td id="LC500" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>   mutate(p_pred = cumsum(dp_pred) + p_lag[1]) %&gt;%</span></td>
      </tr>
      <tr>
        <td id="L501" class="blob-num js-line-number" data-line-number="501"></td>
        <td id="LC501" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>   mutate(e = p - p_pred) %&gt;%</span></td>
      </tr>
      <tr>
        <td id="L502" class="blob-num js-line-number" data-line-number="502"></td>
        <td id="LC502" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>   ungroup()</span></td>
      </tr>
      <tr>
        <td id="L503" class="blob-num js-line-number" data-line-number="503"></td>
        <td id="LC503" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span></span></td>
      </tr>
      <tr>
        <td id="L504" class="blob-num js-line-number" data-line-number="504"></td>
        <td id="LC504" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> summary_holdout_var &lt;- forecast_var2 %&gt;%</span></td>
      </tr>
      <tr>
        <td id="L505" class="blob-num js-line-number" data-line-number="505"></td>
        <td id="LC505" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>   get_MSE_from_forecast(groupby = c(&quot;.model&quot;))</span></td>
      </tr>
      <tr>
        <td id="L506" class="blob-num js-line-number" data-line-number="506"></td>
        <td id="LC506" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> summary_holdout_var</span></td>
      </tr>
      <tr>
        <td id="L507" class="blob-num js-line-number" data-line-number="507"></td>
        <td id="LC507" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L508" class="blob-num js-line-number" data-line-number="508"></td>
        <td id="LC508" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L509" class="blob-num js-line-number" data-line-number="509"></td>
        <td id="LC509" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>############################</span></td>
      </tr>
      <tr>
        <td id="L510" class="blob-num js-line-number" data-line-number="510"></td>
        <td id="LC510" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> GRAPHS part 3</span></td>
      </tr>
      <tr>
        <td id="L511" class="blob-num js-line-number" data-line-number="511"></td>
        <td id="LC511" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>############################</span></td>
      </tr>
      <tr>
        <td id="L512" class="blob-num js-line-number" data-line-number="512"></td>
        <td id="LC512" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> graph actual vs prediction from best arima</span></td>
      </tr>
      <tr>
        <td id="L513" class="blob-num js-line-number" data-line-number="513"></td>
        <td id="LC513" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">conf_level_chr</span> <span class="pl-k">&lt;-</span> paste0(as.character(<span class="pl-smi">conf_level</span>),<span class="pl-s"><span class="pl-pds">&quot;</span>%<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L514" class="blob-num js-line-number" data-line-number="514"></td>
        <td id="LC514" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data_plot</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L515" class="blob-num js-line-number" data-line-number="515"></td>
        <td id="LC515" class="blob-code blob-code-inner js-file-line">  left_join(unnest(<span class="pl-smi">forecast_holdout_best_arima_p</span>[,c(<span class="pl-s"><span class="pl-pds">&quot;</span>date<span class="pl-pds">&quot;</span></span>,<span class="pl-s"><span class="pl-pds">&quot;</span>p_pred<span class="pl-pds">&quot;</span></span>,<span class="pl-smi">conf_level_chr</span>)])) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L516" class="blob-num js-line-number" data-line-number="516"></td>
        <td id="LC516" class="blob-code blob-code-inner js-file-line">  filter(year(<span class="pl-smi">date</span>)<span class="pl-k">&gt;</span><span class="pl-k">=</span><span class="pl-c1">2015</span>)</td>
      </tr>
      <tr>
        <td id="L517" class="blob-num js-line-number" data-line-number="517"></td>
        <td id="LC517" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L518" class="blob-num js-line-number" data-line-number="518"></td>
        <td id="LC518" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L519" class="blob-num js-line-number" data-line-number="519"></td>
        <td id="LC519" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">pred_p_plot</span> <span class="pl-k">&lt;-</span> ggplot(<span class="pl-v">data</span> <span class="pl-k">=</span> <span class="pl-smi">data_plot</span> , aes(<span class="pl-v">x</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>, <span class="pl-v">y</span> <span class="pl-k">=</span> <span class="pl-smi">p</span>))<span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L520" class="blob-num js-line-number" data-line-number="520"></td>
        <td id="LC520" class="blob-code blob-code-inner js-file-line">  geom_line(<span class="pl-v">size</span> <span class="pl-k">=</span> <span class="pl-c1">0.8</span>, aes(<span class="pl-v">color</span> <span class="pl-k">=</span> <span class="pl-s"><span class="pl-pds">&quot;</span>Actual<span class="pl-pds">&quot;</span></span>)) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L521" class="blob-num js-line-number" data-line-number="521"></td>
        <td id="LC521" class="blob-code blob-code-inner js-file-line">  geom_line(aes(<span class="pl-v">x</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>, <span class="pl-v">y</span> <span class="pl-k">=</span> <span class="pl-smi">p_pred</span>, <span class="pl-v">color</span> <span class="pl-k">=</span> <span class="pl-s"><span class="pl-pds">&quot;</span>Prediction <span class="pl-pds">&quot;</span></span>),  <span class="pl-v">size</span> <span class="pl-k">=</span> <span class="pl-c1">1</span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L522" class="blob-num js-line-number" data-line-number="522"></td>
        <td id="LC522" class="blob-code blob-code-inner js-file-line">  <span class="pl-c"><span class="pl-c">#</span>annotate(&quot;text&quot;, x = yearmonth(&quot;2017-08&quot;), y = 257, label = &quot;Prediction &quot;, size=2.5, vjust = 2, color = color[2])+  </span></td>
      </tr>
      <tr>
        <td id="L523" class="blob-num js-line-number" data-line-number="523"></td>
        <td id="LC523" class="blob-code blob-code-inner js-file-line">  <span class="pl-c"><span class="pl-c">#</span>annotate(&quot;text&quot;, x = yearmonth(&quot;2017-03&quot;), y = 258, label = &quot;Actual&quot;, size=2.5, hjust = 1.5, color = color[1])+  </span></td>
      </tr>
      <tr>
        <td id="L524" class="blob-num js-line-number" data-line-number="524"></td>
        <td id="LC524" class="blob-code blob-code-inner js-file-line">  ylab(<span class="pl-s"><span class="pl-pds">&quot;</span>Case-Shiller Price index<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L525" class="blob-num js-line-number" data-line-number="525"></td>
        <td id="LC525" class="blob-code blob-code-inner js-file-line">  xlab(<span class="pl-s"><span class="pl-pds">&quot;</span>Date (month)<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L526" class="blob-num js-line-number" data-line-number="526"></td>
        <td id="LC526" class="blob-code blob-code-inner js-file-line">  scale_color_manual(<span class="pl-v">name</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span><span class="pl-pds">&quot;</span></span>,<span class="pl-v">values</span><span class="pl-k">=</span>c(<span class="pl-smi">color</span>[<span class="pl-c1">1</span>], <span class="pl-smi">color</span>[<span class="pl-c1">2</span>])) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L527" class="blob-num js-line-number" data-line-number="527"></td>
        <td id="LC527" class="blob-code blob-code-inner js-file-line">  scale_x_date(<span class="pl-v">date_breaks</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span>1 years<span class="pl-pds">&quot;</span></span>, <span class="pl-v">labels</span> <span class="pl-k">=</span> date_format(<span class="pl-s"><span class="pl-pds">&quot;</span>%b%Y<span class="pl-pds">&quot;</span></span>)) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L528" class="blob-num js-line-number" data-line-number="528"></td>
        <td id="LC528" class="blob-code blob-code-inner js-file-line">  theme_bg()<span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L529" class="blob-num js-line-number" data-line-number="529"></td>
        <td id="LC529" class="blob-code blob-code-inner js-file-line">  theme(<span class="pl-v">legend.position</span><span class="pl-k">=</span>c(<span class="pl-c1">0.7</span>,<span class="pl-c1">0.1</span>),</td>
      </tr>
      <tr>
        <td id="L530" class="blob-num js-line-number" data-line-number="530"></td>
        <td id="LC530" class="blob-code blob-code-inner js-file-line">        <span class="pl-v">legend.direction</span> <span class="pl-k">=</span> <span class="pl-s"><span class="pl-pds">&quot;</span>horizontal<span class="pl-pds">&quot;</span></span>,</td>
      </tr>
      <tr>
        <td id="L531" class="blob-num js-line-number" data-line-number="531"></td>
        <td id="LC531" class="blob-code blob-code-inner js-file-line">        <span class="pl-v">legend.text</span> <span class="pl-k">=</span> element_text(<span class="pl-v">size</span> <span class="pl-k">=</span> <span class="pl-c1">4</span>),</td>
      </tr>
      <tr>
        <td id="L532" class="blob-num js-line-number" data-line-number="532"></td>
        <td id="LC532" class="blob-code blob-code-inner js-file-line">        <span class="pl-v">legend.key.width</span> <span class="pl-k">=</span> unit(<span class="pl-smi">.8</span>, <span class="pl-s"><span class="pl-pds">&quot;</span>cm<span class="pl-pds">&quot;</span></span>),</td>
      </tr>
      <tr>
        <td id="L533" class="blob-num js-line-number" data-line-number="533"></td>
        <td id="LC533" class="blob-code blob-code-inner js-file-line">        <span class="pl-v">legend.key.height</span> <span class="pl-k">=</span> unit(<span class="pl-smi">.2</span>, <span class="pl-s"><span class="pl-pds">&quot;</span>cm<span class="pl-pds">&quot;</span></span>)) <span class="pl-k">+</span> </td>
      </tr>
      <tr>
        <td id="L534" class="blob-num js-line-number" data-line-number="534"></td>
        <td id="LC534" class="blob-code blob-code-inner js-file-line">  guides(<span class="pl-v">linetype</span> <span class="pl-k">=</span> guide_legend(<span class="pl-v">override.aes</span> <span class="pl-k">=</span> <span class="pl-k">list</span>(<span class="pl-v">size</span> <span class="pl-k">=</span> <span class="pl-c1">0.6</span>)))</td>
      </tr>
      <tr>
        <td id="L535" class="blob-num js-line-number" data-line-number="535"></td>
        <td id="LC535" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">pred_p_plot</span></td>
      </tr>
      <tr>
        <td id="L536" class="blob-num js-line-number" data-line-number="536"></td>
        <td id="LC536" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>save_fig(&quot;pred_p_mp_R&quot;, output, &quot;small&quot;)</span></td>
      </tr>
      <tr>
        <td id="L537" class="blob-num js-line-number" data-line-number="537"></td>
        <td id="LC537" class="blob-code blob-code-inner js-file-line">save_fig(<span class="pl-s"><span class="pl-pds">&quot;</span>ch18-figure-9a-pred-p-mp<span class="pl-pds">&quot;</span></span>, <span class="pl-smi">output</span>, <span class="pl-s"><span class="pl-pds">&quot;</span>small<span class="pl-pds">&quot;</span></span>)  </td>
      </tr>
      <tr>
        <td id="L538" class="blob-num js-line-number" data-line-number="538"></td>
        <td id="LC538" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L539" class="blob-num js-line-number" data-line-number="539"></td>
        <td id="LC539" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L540" class="blob-num js-line-number" data-line-number="540"></td>
        <td id="LC540" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> with uncertainty fan</span></td>
      </tr>
      <tr>
        <td id="L541" class="blob-num js-line-number" data-line-number="541"></td>
        <td id="LC541" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">pred_p_mp_fan_R</span> <span class="pl-k">&lt;-</span> ggplot(<span class="pl-v">data</span> <span class="pl-k">=</span> <span class="pl-smi">data_plot</span> , aes(<span class="pl-v">x</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>, <span class="pl-v">y</span> <span class="pl-k">=</span> <span class="pl-smi">p</span>))<span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L542" class="blob-num js-line-number" data-line-number="542"></td>
        <td id="LC542" class="blob-code blob-code-inner js-file-line">  geom_line(<span class="pl-v">size</span> <span class="pl-k">=</span> <span class="pl-c1">0.8</span>, aes(<span class="pl-v">color</span> <span class="pl-k">=</span> <span class="pl-s"><span class="pl-pds">&quot;</span>Actual<span class="pl-pds">&quot;</span></span>)) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L543" class="blob-num js-line-number" data-line-number="543"></td>
        <td id="LC543" class="blob-code blob-code-inner js-file-line">  geom_line(aes(<span class="pl-v">x</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>, <span class="pl-v">y</span> <span class="pl-k">=</span> <span class="pl-smi">p_pred</span>, <span class="pl-v">color</span> <span class="pl-k">=</span> <span class="pl-s"><span class="pl-pds">&quot;</span>Prediction <span class="pl-pds">&quot;</span></span>),  <span class="pl-v">size</span> <span class="pl-k">=</span> <span class="pl-c1">1</span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L544" class="blob-num js-line-number" data-line-number="544"></td>
        <td id="LC544" class="blob-code blob-code-inner js-file-line">  geom_ribbon(aes(<span class="pl-v">ymin</span> <span class="pl-k">=</span> <span class="pl-smi">.lower</span>, <span class="pl-v">ymax</span> <span class="pl-k">=</span> <span class="pl-smi">.upper</span>), <span class="pl-v">alpha</span><span class="pl-k">=</span><span class="pl-c1">0.2</span>,   <span class="pl-v">bg</span><span class="pl-k">=</span><span class="pl-smi">color</span>[<span class="pl-c1">2</span>]) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L545" class="blob-num js-line-number" data-line-number="545"></td>
        <td id="LC545" class="blob-code blob-code-inner js-file-line">  ylab(<span class="pl-s"><span class="pl-pds">&quot;</span>Case-Shiller Price index<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L546" class="blob-num js-line-number" data-line-number="546"></td>
        <td id="LC546" class="blob-code blob-code-inner js-file-line">  xlab(<span class="pl-s"><span class="pl-pds">&quot;</span>Date (month)<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L547" class="blob-num js-line-number" data-line-number="547"></td>
        <td id="LC547" class="blob-code blob-code-inner js-file-line">  scale_color_manual(<span class="pl-v">name</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span><span class="pl-pds">&quot;</span></span>,<span class="pl-v">values</span><span class="pl-k">=</span>c(<span class="pl-smi">color</span>[<span class="pl-c1">1</span>], <span class="pl-smi">color</span>[<span class="pl-c1">2</span>])) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L548" class="blob-num js-line-number" data-line-number="548"></td>
        <td id="LC548" class="blob-code blob-code-inner js-file-line">  scale_x_date(<span class="pl-v">date_breaks</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span>1 years<span class="pl-pds">&quot;</span></span>, <span class="pl-v">labels</span> <span class="pl-k">=</span> date_format(<span class="pl-s"><span class="pl-pds">&quot;</span>%b%Y<span class="pl-pds">&quot;</span></span>)) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L549" class="blob-num js-line-number" data-line-number="549"></td>
        <td id="LC549" class="blob-code blob-code-inner js-file-line">  theme_bg()<span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L550" class="blob-num js-line-number" data-line-number="550"></td>
        <td id="LC550" class="blob-code blob-code-inner js-file-line">  theme(<span class="pl-v">legend.position</span><span class="pl-k">=</span>c(<span class="pl-c1">0.7</span>,<span class="pl-c1">0.1</span>),</td>
      </tr>
      <tr>
        <td id="L551" class="blob-num js-line-number" data-line-number="551"></td>
        <td id="LC551" class="blob-code blob-code-inner js-file-line">        <span class="pl-v">legend.direction</span> <span class="pl-k">=</span> <span class="pl-s"><span class="pl-pds">&quot;</span>horizontal<span class="pl-pds">&quot;</span></span>,</td>
      </tr>
      <tr>
        <td id="L552" class="blob-num js-line-number" data-line-number="552"></td>
        <td id="LC552" class="blob-code blob-code-inner js-file-line">        <span class="pl-v">legend.text</span> <span class="pl-k">=</span> element_text(<span class="pl-v">size</span> <span class="pl-k">=</span> <span class="pl-c1">4</span>),</td>
      </tr>
      <tr>
        <td id="L553" class="blob-num js-line-number" data-line-number="553"></td>
        <td id="LC553" class="blob-code blob-code-inner js-file-line">        <span class="pl-v">legend.key.width</span> <span class="pl-k">=</span> unit(<span class="pl-smi">.8</span>, <span class="pl-s"><span class="pl-pds">&quot;</span>cm<span class="pl-pds">&quot;</span></span>),</td>
      </tr>
      <tr>
        <td id="L554" class="blob-num js-line-number" data-line-number="554"></td>
        <td id="LC554" class="blob-code blob-code-inner js-file-line">        <span class="pl-v">legend.key.height</span> <span class="pl-k">=</span> unit(<span class="pl-smi">.2</span>, <span class="pl-s"><span class="pl-pds">&quot;</span>cm<span class="pl-pds">&quot;</span></span>)) <span class="pl-k">+</span> </td>
      </tr>
      <tr>
        <td id="L555" class="blob-num js-line-number" data-line-number="555"></td>
        <td id="LC555" class="blob-code blob-code-inner js-file-line">  guides(<span class="pl-v">linetype</span> <span class="pl-k">=</span> guide_legend(<span class="pl-v">override.aes</span> <span class="pl-k">=</span> <span class="pl-k">list</span>(<span class="pl-v">size</span> <span class="pl-k">=</span> <span class="pl-c1">0.6</span>)))</td>
      </tr>
      <tr>
        <td id="L556" class="blob-num js-line-number" data-line-number="556"></td>
        <td id="LC556" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">pred_p_mp_fan_R</span></td>
      </tr>
      <tr>
        <td id="L557" class="blob-num js-line-number" data-line-number="557"></td>
        <td id="LC557" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>save_fig(&quot;pred_p_mp_fan_R&quot;, output, &quot;small&quot;)</span></td>
      </tr>
      <tr>
        <td id="L558" class="blob-num js-line-number" data-line-number="558"></td>
        <td id="LC558" class="blob-code blob-code-inner js-file-line">save_fig(<span class="pl-s"><span class="pl-pds">&quot;</span>ch18-figure-9b-pred-p-mp-fan<span class="pl-pds">&quot;</span></span>, <span class="pl-smi">output</span>, <span class="pl-s"><span class="pl-pds">&quot;</span>small<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L559" class="blob-num js-line-number" data-line-number="559"></td>
        <td id="LC559" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L560" class="blob-num js-line-number" data-line-number="560"></td>
        <td id="LC560" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>##########################</span></td>
      </tr>
      <tr>
        <td id="L561" class="blob-num js-line-number" data-line-number="561"></td>
        <td id="LC561" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> EXTERNAL VALIDITY</span></td>
      </tr>
      <tr>
        <td id="L562" class="blob-num js-line-number" data-line-number="562"></td>
        <td id="LC562" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> do the prediction for an extra year</span></td>
      </tr>
      <tr>
        <td id="L563" class="blob-num js-line-number" data-line-number="563"></td>
        <td id="LC563" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>##########################</span></td>
      </tr>
      <tr>
        <td id="L564" class="blob-num js-line-number" data-line-number="564"></td>
        <td id="LC564" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L565" class="blob-num js-line-number" data-line-number="565"></td>
        <td id="LC565" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L566" class="blob-num js-line-number" data-line-number="566"></td>
        <td id="LC566" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data</span> <span class="pl-k">&lt;-</span> read_rds(paste0(<span class="pl-smi">data_in</span>,<span class="pl-s"><span class="pl-pds">&quot;</span>houseprices-data-1990-2018.rds<span class="pl-pds">&quot;</span></span>))</td>
      </tr>
      <tr>
        <td id="L567" class="blob-num js-line-number" data-line-number="567"></td>
        <td id="LC567" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L568" class="blob-num js-line-number" data-line-number="568"></td>
        <td id="LC568" class="blob-code blob-code-inner js-file-line">  filter(<span class="pl-smi">date</span><span class="pl-k">&gt;</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span>2000-01-01<span class="pl-pds">&quot;</span></span> <span class="pl-k">&amp;</span> <span class="pl-smi">date</span><span class="pl-k">&lt;</span><span class="pl-s"><span class="pl-pds">&quot;</span>2019-01-01<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L569" class="blob-num js-line-number" data-line-number="569"></td>
        <td id="LC569" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> pick if seasonal or non seasonal version used, will be cut later</span></td>
      </tr>
      <tr>
        <td id="L570" class="blob-num js-line-number" data-line-number="570"></td>
        <td id="LC570" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L571" class="blob-num js-line-number" data-line-number="571"></td>
        <td id="LC571" class="blob-code blob-code-inner js-file-line">  mutate(</td>
      </tr>
      <tr>
        <td id="L572" class="blob-num js-line-number" data-line-number="572"></td>
        <td id="LC572" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">p</span><span class="pl-k">=</span><span class="pl-smi">pn</span>,</td>
      </tr>
      <tr>
        <td id="L573" class="blob-num js-line-number" data-line-number="573"></td>
        <td id="LC573" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">u</span><span class="pl-k">=</span><span class="pl-smi">us</span>,</td>
      </tr>
      <tr>
        <td id="L574" class="blob-num js-line-number" data-line-number="574"></td>
        <td id="LC574" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">emp</span><span class="pl-k">=</span><span class="pl-smi">emps</span>,</td>
      </tr>
      <tr>
        <td id="L575" class="blob-num js-line-number" data-line-number="575"></td>
        <td id="LC575" class="blob-code blob-code-inner js-file-line">  )</td>
      </tr>
      <tr>
        <td id="L576" class="blob-num js-line-number" data-line-number="576"></td>
        <td id="LC576" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L577" class="blob-num js-line-number" data-line-number="577"></td>
        <td id="LC577" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L578" class="blob-num js-line-number" data-line-number="578"></td>
        <td id="LC578" class="blob-code blob-code-inner js-file-line">  mutate(</td>
      </tr>
      <tr>
        <td id="L579" class="blob-num js-line-number" data-line-number="579"></td>
        <td id="LC579" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">dp</span>   <span class="pl-k">=</span> difference(<span class="pl-smi">p</span>, <span class="pl-v">lag</span><span class="pl-k">=</span><span class="pl-c1">1</span>, <span class="pl-v">order_by</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>),</td>
      </tr>
      <tr>
        <td id="L580" class="blob-num js-line-number" data-line-number="580"></td>
        <td id="LC580" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">p_lag</span> <span class="pl-k">=</span> lag(<span class="pl-smi">p</span>),</td>
      </tr>
      <tr>
        <td id="L581" class="blob-num js-line-number" data-line-number="581"></td>
        <td id="LC581" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">lnp</span> <span class="pl-k">=</span> log(<span class="pl-smi">p</span>),</td>
      </tr>
      <tr>
        <td id="L582" class="blob-num js-line-number" data-line-number="582"></td>
        <td id="LC582" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">dlnp</span>   <span class="pl-k">=</span> difference(<span class="pl-smi">lnp</span>, <span class="pl-v">lag</span><span class="pl-k">=</span><span class="pl-c1">1</span>, <span class="pl-v">order_by</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>),</td>
      </tr>
      <tr>
        <td id="L583" class="blob-num js-line-number" data-line-number="583"></td>
        <td id="LC583" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">lnp_lag</span> <span class="pl-k">=</span> lag(<span class="pl-smi">lnp</span>),</td>
      </tr>
      <tr>
        <td id="L584" class="blob-num js-line-number" data-line-number="584"></td>
        <td id="LC584" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">dlnp_lag</span>   <span class="pl-k">=</span> lag(<span class="pl-smi">dlnp</span>),</td>
      </tr>
      <tr>
        <td id="L585" class="blob-num js-line-number" data-line-number="585"></td>
        <td id="LC585" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">du</span>     <span class="pl-k">=</span> difference(<span class="pl-smi">u</span>, <span class="pl-v">lag</span><span class="pl-k">=</span><span class="pl-c1">1</span>, <span class="pl-v">order_by</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>),</td>
      </tr>
      <tr>
        <td id="L586" class="blob-num js-line-number" data-line-number="586"></td>
        <td id="LC586" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">lnemp</span> <span class="pl-k">=</span> log(<span class="pl-smi">emp</span>),</td>
      </tr>
      <tr>
        <td id="L587" class="blob-num js-line-number" data-line-number="587"></td>
        <td id="LC587" class="blob-code blob-code-inner js-file-line">    <span class="pl-v">dlnemp</span> <span class="pl-k">=</span> difference(<span class="pl-smi">lnemp</span>, <span class="pl-v">lag</span><span class="pl-k">=</span><span class="pl-c1">1</span>, <span class="pl-v">order_by</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>)</td>
      </tr>
      <tr>
        <td id="L588" class="blob-num js-line-number" data-line-number="588"></td>
        <td id="LC588" class="blob-code blob-code-inner js-file-line">  )</td>
      </tr>
      <tr>
        <td id="L589" class="blob-num js-line-number" data-line-number="589"></td>
        <td id="LC589" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L590" class="blob-num js-line-number" data-line-number="590"></td>
        <td id="LC590" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L591" class="blob-num js-line-number" data-line-number="591"></td>
        <td id="LC591" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Last year of data</span></td>
      </tr>
      <tr>
        <td id="L592" class="blob-num js-line-number" data-line-number="592"></td>
        <td id="LC592" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data_holdout</span><span class="pl-k">&lt;-</span> <span class="pl-smi">data</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L593" class="blob-num js-line-number" data-line-number="593"></td>
        <td id="LC593" class="blob-code blob-code-inner js-file-line">  slice((n()<span class="pl-k">-</span><span class="pl-c1">11</span>)<span class="pl-k">:</span>n())</td>
      </tr>
      <tr>
        <td id="L594" class="blob-num js-line-number" data-line-number="594"></td>
        <td id="LC594" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L595" class="blob-num js-line-number" data-line-number="595"></td>
        <td id="LC595" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> Rest of data for training</span></td>
      </tr>
      <tr>
        <td id="L596" class="blob-num js-line-number" data-line-number="596"></td>
        <td id="LC596" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data_train</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L597" class="blob-num js-line-number" data-line-number="597"></td>
        <td id="LC597" class="blob-code blob-code-inner js-file-line">  slice(<span class="pl-c1">1</span><span class="pl-k">:</span>(n()<span class="pl-k">-</span><span class="pl-c1">12</span>))</td>
      </tr>
      <tr>
        <td id="L598" class="blob-num js-line-number" data-line-number="598"></td>
        <td id="LC598" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L599" class="blob-num js-line-number" data-line-number="599"></td>
        <td id="LC599" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L600" class="blob-num js-line-number" data-line-number="600"></td>
        <td id="LC600" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>##########################x</span></td>
      </tr>
      <tr>
        <td id="L601" class="blob-num js-line-number" data-line-number="601"></td>
        <td id="LC601" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> predict for holdout</span></td>
      </tr>
      <tr>
        <td id="L602" class="blob-num js-line-number" data-line-number="602"></td>
        <td id="LC602" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>##########################x</span></td>
      </tr>
      <tr>
        <td id="L603" class="blob-num js-line-number" data-line-number="603"></td>
        <td id="LC603" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L604" class="blob-num js-line-number" data-line-number="604"></td>
        <td id="LC604" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> best model when p is target</span></td>
      </tr>
      <tr>
        <td id="L605" class="blob-num js-line-number" data-line-number="605"></td>
        <td id="LC605" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> (need to adjust code if lnp is best)</span></td>
      </tr>
      <tr>
        <td id="L606" class="blob-num js-line-number" data-line-number="606"></td>
        <td id="LC606" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L607" class="blob-num js-line-number" data-line-number="607"></td>
        <td id="LC607" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">best_arima_model_p</span> <span class="pl-k">&lt;-</span> <span class="pl-s"><span class="pl-pds">&quot;</span>m4<span class="pl-pds">&quot;</span></span></td>
      </tr>
      <tr>
        <td id="L608" class="blob-num js-line-number" data-line-number="608"></td>
        <td id="LC608" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L609" class="blob-num js-line-number" data-line-number="609"></td>
        <td id="LC609" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> re-estimate best models on full train set</span></td>
      </tr>
      <tr>
        <td id="L610" class="blob-num js-line-number" data-line-number="610"></td>
        <td id="LC610" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">models_best_arima_p</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data_train</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L611" class="blob-num js-line-number" data-line-number="611"></td>
        <td id="LC611" class="blob-code blob-code-inner js-file-line">  model(<span class="pl-v">best</span> <span class="pl-k">=</span> get(<span class="pl-smi">best_arima_model_p</span>))</td>
      </tr>
      <tr>
        <td id="L612" class="blob-num js-line-number" data-line-number="612"></td>
        <td id="LC612" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L613" class="blob-num js-line-number" data-line-number="613"></td>
        <td id="LC613" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">rmse_train_best_p</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">models_best_arima_p</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L614" class="blob-num js-line-number" data-line-number="614"></td>
        <td id="LC614" class="blob-code blob-code-inner js-file-line">  get_RMSE_from_model(<span class="pl-v">groupby</span> <span class="pl-k">=</span> c(<span class="pl-s"><span class="pl-pds">&quot;</span>.model<span class="pl-pds">&quot;</span></span>))</td>
      </tr>
      <tr>
        <td id="L615" class="blob-num js-line-number" data-line-number="615"></td>
        <td id="LC615" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L616" class="blob-num js-line-number" data-line-number="616"></td>
        <td id="LC616" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">forecast_holdout_best_arima_p</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">models_best_arima_p</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L617" class="blob-num js-line-number" data-line-number="617"></td>
        <td id="LC617" class="blob-code blob-code-inner js-file-line">  forecast(<span class="pl-v">h</span><span class="pl-k">=</span><span class="pl-c1">12</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L618" class="blob-num js-line-number" data-line-number="618"></td>
        <td id="LC618" class="blob-code blob-code-inner js-file-line">  as_tsibble() %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L619" class="blob-num js-line-number" data-line-number="619"></td>
        <td id="LC619" class="blob-code blob-code-inner js-file-line">  rename(<span class="pl-v">p_pred</span> <span class="pl-k">=</span> <span class="pl-smi">p</span>)  %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L620" class="blob-num js-line-number" data-line-number="620"></td>
        <td id="LC620" class="blob-code blob-code-inner js-file-line">  left_join(<span class="pl-smi">data_holdout</span>[,c(<span class="pl-s"><span class="pl-pds">&quot;</span>date<span class="pl-pds">&quot;</span></span>,<span class="pl-s"><span class="pl-pds">&quot;</span>p<span class="pl-pds">&quot;</span></span>)]) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L621" class="blob-num js-line-number" data-line-number="621"></td>
        <td id="LC621" class="blob-code blob-code-inner js-file-line">  mutate(<span class="pl-v">e</span> <span class="pl-k">=</span> <span class="pl-smi">p</span> <span class="pl-k">-</span> <span class="pl-smi">p_pred</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L622" class="blob-num js-line-number" data-line-number="622"></td>
        <td id="LC622" class="blob-code blob-code-inner js-file-line">  ungroup()</td>
      </tr>
      <tr>
        <td id="L623" class="blob-num js-line-number" data-line-number="623"></td>
        <td id="LC623" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L624" class="blob-num js-line-number" data-line-number="624"></td>
        <td id="LC624" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">summary_holdout_best_arima_p</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">forecast_holdout_best_arima_p</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L625" class="blob-num js-line-number" data-line-number="625"></td>
        <td id="LC625" class="blob-code blob-code-inner js-file-line">  get_MSE_from_forecast(<span class="pl-v">groupby</span> <span class="pl-k">=</span> c(<span class="pl-s"><span class="pl-pds">&quot;</span>.model<span class="pl-pds">&quot;</span></span>))</td>
      </tr>
      <tr>
        <td id="L626" class="blob-num js-line-number" data-line-number="626"></td>
        <td id="LC626" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">summary_holdout_best_arima_p</span></td>
      </tr>
      <tr>
        <td id="L627" class="blob-num js-line-number" data-line-number="627"></td>
        <td id="LC627" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L628" class="blob-num js-line-number" data-line-number="628"></td>
        <td id="LC628" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L629" class="blob-num js-line-number" data-line-number="629"></td>
        <td id="LC629" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>############################</span></td>
      </tr>
      <tr>
        <td id="L630" class="blob-num js-line-number" data-line-number="630"></td>
        <td id="LC630" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> GRAPHS part 4</span></td>
      </tr>
      <tr>
        <td id="L631" class="blob-num js-line-number" data-line-number="631"></td>
        <td id="LC631" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>############################</span></td>
      </tr>
      <tr>
        <td id="L632" class="blob-num js-line-number" data-line-number="632"></td>
        <td id="LC632" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> graph from 2015-18, actual vs prediction from best arima</span></td>
      </tr>
      <tr>
        <td id="L633" class="blob-num js-line-number" data-line-number="633"></td>
        <td id="LC633" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L634" class="blob-num js-line-number" data-line-number="634"></td>
        <td id="LC634" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L635" class="blob-num js-line-number" data-line-number="635"></td>
        <td id="LC635" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>#</span></td>
      </tr>
      <tr>
        <td id="L636" class="blob-num js-line-number" data-line-number="636"></td>
        <td id="LC636" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">conf_level</span> <span class="pl-k">&lt;-</span>  <span class="pl-c1">80</span></td>
      </tr>
      <tr>
        <td id="L637" class="blob-num js-line-number" data-line-number="637"></td>
        <td id="LC637" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L638" class="blob-num js-line-number" data-line-number="638"></td>
        <td id="LC638" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> best model when p is target</span></td>
      </tr>
      <tr>
        <td id="L639" class="blob-num js-line-number" data-line-number="639"></td>
        <td id="LC639" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> (need to adjust code if lnp is best)</span></td>
      </tr>
      <tr>
        <td id="L640" class="blob-num js-line-number" data-line-number="640"></td>
        <td id="LC640" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L641" class="blob-num js-line-number" data-line-number="641"></td>
        <td id="LC641" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">best_arima_model_p</span> <span class="pl-k">&lt;-</span> <span class="pl-s"><span class="pl-pds">&quot;</span>m4_p<span class="pl-pds">&quot;</span></span></td>
      </tr>
      <tr>
        <td id="L642" class="blob-num js-line-number" data-line-number="642"></td>
        <td id="LC642" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L643" class="blob-num js-line-number" data-line-number="643"></td>
        <td id="LC643" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> re-estimate best models on full train set</span></td>
      </tr>
      <tr>
        <td id="L644" class="blob-num js-line-number" data-line-number="644"></td>
        <td id="LC644" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">models_best_arima_p</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data_train</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L645" class="blob-num js-line-number" data-line-number="645"></td>
        <td id="LC645" class="blob-code blob-code-inner js-file-line">  model(<span class="pl-v">best</span> <span class="pl-k">=</span> get(<span class="pl-smi">best_arima_model_p</span>))</td>
      </tr>
      <tr>
        <td id="L646" class="blob-num js-line-number" data-line-number="646"></td>
        <td id="LC646" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L647" class="blob-num js-line-number" data-line-number="647"></td>
        <td id="LC647" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">rmse_train_best_p</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">models_best_arima_p</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L648" class="blob-num js-line-number" data-line-number="648"></td>
        <td id="LC648" class="blob-code blob-code-inner js-file-line">  get_RMSE_from_model(<span class="pl-v">groupby</span> <span class="pl-k">=</span> c(<span class="pl-s"><span class="pl-pds">&quot;</span>.model<span class="pl-pds">&quot;</span></span>))</td>
      </tr>
      <tr>
        <td id="L649" class="blob-num js-line-number" data-line-number="649"></td>
        <td id="LC649" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L650" class="blob-num js-line-number" data-line-number="650"></td>
        <td id="LC650" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">forecast_holdout_best_arima_p</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">models_best_arima_p</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L651" class="blob-num js-line-number" data-line-number="651"></td>
        <td id="LC651" class="blob-code blob-code-inner js-file-line">  forecast(<span class="pl-v">h</span><span class="pl-k">=</span><span class="pl-c1">12</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L652" class="blob-num js-line-number" data-line-number="652"></td>
        <td id="LC652" class="blob-code blob-code-inner js-file-line">  hilo(<span class="pl-v">level</span> <span class="pl-k">=</span> c(<span class="pl-smi">conf_level</span>)) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L653" class="blob-num js-line-number" data-line-number="653"></td>
        <td id="LC653" class="blob-code blob-code-inner js-file-line">  as_tsibble() %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L654" class="blob-num js-line-number" data-line-number="654"></td>
        <td id="LC654" class="blob-code blob-code-inner js-file-line">  rename(<span class="pl-v">p_pred</span> <span class="pl-k">=</span> <span class="pl-smi">p</span>)  %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L655" class="blob-num js-line-number" data-line-number="655"></td>
        <td id="LC655" class="blob-code blob-code-inner js-file-line">  left_join(<span class="pl-smi">data_holdout</span>[,c(<span class="pl-s"><span class="pl-pds">&quot;</span>date<span class="pl-pds">&quot;</span></span>,<span class="pl-s"><span class="pl-pds">&quot;</span>p<span class="pl-pds">&quot;</span></span>)]) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L656" class="blob-num js-line-number" data-line-number="656"></td>
        <td id="LC656" class="blob-code blob-code-inner js-file-line">  mutate(<span class="pl-v">e</span> <span class="pl-k">=</span> <span class="pl-smi">p</span> <span class="pl-k">-</span> <span class="pl-smi">p_pred</span>) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L657" class="blob-num js-line-number" data-line-number="657"></td>
        <td id="LC657" class="blob-code blob-code-inner js-file-line">  ungroup()</td>
      </tr>
      <tr>
        <td id="L658" class="blob-num js-line-number" data-line-number="658"></td>
        <td id="LC658" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L659" class="blob-num js-line-number" data-line-number="659"></td>
        <td id="LC659" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">summary_holdout_best_arima_p</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">forecast_holdout_best_arima_p</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L660" class="blob-num js-line-number" data-line-number="660"></td>
        <td id="LC660" class="blob-code blob-code-inner js-file-line">  get_MSE_from_forecast(<span class="pl-v">groupby</span> <span class="pl-k">=</span> c(<span class="pl-s"><span class="pl-pds">&quot;</span>.model<span class="pl-pds">&quot;</span></span>))</td>
      </tr>
      <tr>
        <td id="L661" class="blob-num js-line-number" data-line-number="661"></td>
        <td id="LC661" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L662" class="blob-num js-line-number" data-line-number="662"></td>
        <td id="LC662" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> graph actual vs prediction from best arima</span></td>
      </tr>
      <tr>
        <td id="L663" class="blob-num js-line-number" data-line-number="663"></td>
        <td id="LC663" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">conf_level_chr</span> <span class="pl-k">&lt;-</span> paste0(as.character(<span class="pl-smi">conf_level</span>),<span class="pl-s"><span class="pl-pds">&quot;</span>%<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L664" class="blob-num js-line-number" data-line-number="664"></td>
        <td id="LC664" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">data_plot</span> <span class="pl-k">&lt;-</span> <span class="pl-smi">data</span> %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L665" class="blob-num js-line-number" data-line-number="665"></td>
        <td id="LC665" class="blob-code blob-code-inner js-file-line">  left_join(unnest(<span class="pl-smi">forecast_holdout_best_arima_p</span>[,c(<span class="pl-s"><span class="pl-pds">&quot;</span>date<span class="pl-pds">&quot;</span></span>,<span class="pl-s"><span class="pl-pds">&quot;</span>p_pred<span class="pl-pds">&quot;</span></span>,<span class="pl-smi">conf_level_chr</span>)])) %<span class="pl-k">&gt;</span>%</td>
      </tr>
      <tr>
        <td id="L666" class="blob-num js-line-number" data-line-number="666"></td>
        <td id="LC666" class="blob-code blob-code-inner js-file-line">  filter(year(<span class="pl-smi">date</span>)<span class="pl-k">&gt;</span><span class="pl-k">=</span><span class="pl-c1">2015</span>)</td>
      </tr>
      <tr>
        <td id="L667" class="blob-num js-line-number" data-line-number="667"></td>
        <td id="LC667" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L668" class="blob-num js-line-number" data-line-number="668"></td>
        <td id="LC668" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L669" class="blob-num js-line-number" data-line-number="669"></td>
        <td id="LC669" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span> with uncertainty fan</span></td>
      </tr>
      <tr>
        <td id="L670" class="blob-num js-line-number" data-line-number="670"></td>
        <td id="LC670" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">pred_p_mp_fan2018_R</span> <span class="pl-k">&lt;-</span> ggplot(<span class="pl-v">data</span> <span class="pl-k">=</span> <span class="pl-smi">data_plot</span> , aes(<span class="pl-v">x</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>, <span class="pl-v">y</span> <span class="pl-k">=</span> <span class="pl-smi">p</span>))<span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L671" class="blob-num js-line-number" data-line-number="671"></td>
        <td id="LC671" class="blob-code blob-code-inner js-file-line">  geom_line(<span class="pl-v">size</span> <span class="pl-k">=</span> <span class="pl-c1">0.8</span>, aes(<span class="pl-v">color</span> <span class="pl-k">=</span> <span class="pl-s"><span class="pl-pds">&quot;</span>Actual<span class="pl-pds">&quot;</span></span>)) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L672" class="blob-num js-line-number" data-line-number="672"></td>
        <td id="LC672" class="blob-code blob-code-inner js-file-line">  geom_line(aes(<span class="pl-v">x</span> <span class="pl-k">=</span> <span class="pl-smi">date</span>, <span class="pl-v">y</span> <span class="pl-k">=</span> <span class="pl-smi">p_pred</span>, <span class="pl-v">color</span> <span class="pl-k">=</span> <span class="pl-s"><span class="pl-pds">&quot;</span>Prediction <span class="pl-pds">&quot;</span></span>),  <span class="pl-v">size</span> <span class="pl-k">=</span> <span class="pl-c1">1</span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L673" class="blob-num js-line-number" data-line-number="673"></td>
        <td id="LC673" class="blob-code blob-code-inner js-file-line">  geom_ribbon(aes(<span class="pl-v">ymin</span> <span class="pl-k">=</span> <span class="pl-smi">.lower</span>, <span class="pl-v">ymax</span> <span class="pl-k">=</span> <span class="pl-smi">.upper</span>), <span class="pl-v">alpha</span><span class="pl-k">=</span><span class="pl-c1">0.2</span>,   <span class="pl-v">bg</span><span class="pl-k">=</span><span class="pl-smi">color</span>[<span class="pl-c1">2</span>]) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L674" class="blob-num js-line-number" data-line-number="674"></td>
        <td id="LC674" class="blob-code blob-code-inner js-file-line">  ylab(<span class="pl-s"><span class="pl-pds">&quot;</span>Case-Shiller Price index<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L675" class="blob-num js-line-number" data-line-number="675"></td>
        <td id="LC675" class="blob-code blob-code-inner js-file-line">  xlab(<span class="pl-s"><span class="pl-pds">&quot;</span>Date (month)<span class="pl-pds">&quot;</span></span>) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L676" class="blob-num js-line-number" data-line-number="676"></td>
        <td id="LC676" class="blob-code blob-code-inner js-file-line">  scale_color_manual(<span class="pl-v">name</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span><span class="pl-pds">&quot;</span></span>, <span class="pl-v">values</span><span class="pl-k">=</span>c(<span class="pl-smi">color</span>[<span class="pl-c1">1</span>], <span class="pl-smi">color</span>[<span class="pl-c1">2</span>])) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L677" class="blob-num js-line-number" data-line-number="677"></td>
        <td id="LC677" class="blob-code blob-code-inner js-file-line">  scale_x_date(<span class="pl-v">date_breaks</span><span class="pl-k">=</span><span class="pl-s"><span class="pl-pds">&quot;</span>1 years<span class="pl-pds">&quot;</span></span>, <span class="pl-v">labels</span> <span class="pl-k">=</span> date_format(<span class="pl-s"><span class="pl-pds">&quot;</span>%b%Y<span class="pl-pds">&quot;</span></span>)) <span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L678" class="blob-num js-line-number" data-line-number="678"></td>
        <td id="LC678" class="blob-code blob-code-inner js-file-line">  theme_bg()<span class="pl-k">+</span></td>
      </tr>
      <tr>
        <td id="L679" class="blob-num js-line-number" data-line-number="679"></td>
        <td id="LC679" class="blob-code blob-code-inner js-file-line">  theme(<span class="pl-v">legend.position</span><span class="pl-k">=</span>c(<span class="pl-c1">0.7</span>,<span class="pl-c1">0.1</span>),</td>
      </tr>
      <tr>
        <td id="L680" class="blob-num js-line-number" data-line-number="680"></td>
        <td id="LC680" class="blob-code blob-code-inner js-file-line">        <span class="pl-v">legend.direction</span> <span class="pl-k">=</span> <span class="pl-s"><span class="pl-pds">&quot;</span>horizontal<span class="pl-pds">&quot;</span></span>,</td>
      </tr>
      <tr>
        <td id="L681" class="blob-num js-line-number" data-line-number="681"></td>
        <td id="LC681" class="blob-code blob-code-inner js-file-line">        <span class="pl-v">legend.text</span> <span class="pl-k">=</span> element_text(<span class="pl-v">size</span> <span class="pl-k">=</span> <span class="pl-c1">4</span>),</td>
      </tr>
      <tr>
        <td id="L682" class="blob-num js-line-number" data-line-number="682"></td>
        <td id="LC682" class="blob-code blob-code-inner js-file-line">        <span class="pl-v">legend.key.width</span> <span class="pl-k">=</span> unit(<span class="pl-smi">.8</span>, <span class="pl-s"><span class="pl-pds">&quot;</span>cm<span class="pl-pds">&quot;</span></span>),</td>
      </tr>
      <tr>
        <td id="L683" class="blob-num js-line-number" data-line-number="683"></td>
        <td id="LC683" class="blob-code blob-code-inner js-file-line">        <span class="pl-v">legend.key.height</span> <span class="pl-k">=</span> unit(<span class="pl-smi">.2</span>, <span class="pl-s"><span class="pl-pds">&quot;</span>cm<span class="pl-pds">&quot;</span></span>)) <span class="pl-k">+</span> </td>
      </tr>
      <tr>
        <td id="L684" class="blob-num js-line-number" data-line-number="684"></td>
        <td id="LC684" class="blob-code blob-code-inner js-file-line">  guides(<span class="pl-v">linetype</span> <span class="pl-k">=</span> guide_legend(<span class="pl-v">override.aes</span> <span class="pl-k">=</span> <span class="pl-k">list</span>(<span class="pl-v">size</span> <span class="pl-k">=</span> <span class="pl-c1">0.6</span>)))</td>
      </tr>
      <tr>
        <td id="L685" class="blob-num js-line-number" data-line-number="685"></td>
        <td id="LC685" class="blob-code blob-code-inner js-file-line"><span class="pl-smi">pred_p_mp_fan2018_R</span></td>
      </tr>
      <tr>
        <td id="L686" class="blob-num js-line-number" data-line-number="686"></td>
        <td id="LC686" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>save_fig(&quot;pred_p_mp_fan2018_R&quot;, output, &quot;small&quot;)</span></td>
      </tr>
      <tr>
        <td id="L687" class="blob-num js-line-number" data-line-number="687"></td>
        <td id="LC687" class="blob-code blob-code-inner js-file-line">save_fig(<span class="pl-s"><span class="pl-pds">&quot;</span>ch18-figure-11-pred-p-mp-fan2018<span class="pl-pds">&quot;</span></span>, <span class="pl-smi">output</span>, <span class="pl-s"><span class="pl-pds">&quot;</span>small<span class="pl-pds">&quot;</span></span>)</td>
      </tr>
      <tr>
        <td id="L688" class="blob-num js-line-number" data-line-number="688"></td>
        <td id="LC688" class="blob-code blob-code-inner js-file-line">
</td>
      </tr>
      <tr>
        <td id="L689" class="blob-num js-line-number" data-line-number="689"></td>
        <td id="LC689" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>ch18-table-1-swim-rmse</span></td>
      </tr>
      <tr>
        <td id="L690" class="blob-num js-line-number" data-line-number="690"></td>
        <td id="LC690" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>ch18-table-2-cs-models-rmse</span></td>
      </tr>
      <tr>
        <td id="L691" class="blob-num js-line-number" data-line-number="691"></td>
        <td id="LC691" class="blob-code blob-code-inner js-file-line"><span class="pl-c"><span class="pl-c">#</span>ch18-table-3-arima-folds</span></td>
      </tr>
</table>

  <details class="details-reset details-overlay BlobToolbar position-absolute js-file-line-actions dropdown d-none" aria-hidden="true">
    <summary class="btn-octicon ml-0 px-2 p-0 bg-white border border-gray-dark rounded-1" aria-label="Inline file action toolbar">
      <svg class="octicon octicon-kebab-horizontal" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path d="M8 9a1.5 1.5 0 100-3 1.5 1.5 0 000 3zM1.5 9a1.5 1.5 0 100-3 1.5 1.5 0 000 3zm13 0a1.5 1.5 0 100-3 1.5 1.5 0 000 3z"></path></svg>
    </summary>
    <details-menu>
      <ul class="BlobToolbar-dropdown dropdown-menu dropdown-menu-se mt-2" style="width:185px">
        <li>
          <clipboard-copy role="menuitem" class="dropdown-item" id="js-copy-lines" style="cursor:pointer;">
            Copy lines
          </clipboard-copy>
        </li>
        <li>
          <clipboard-copy role="menuitem" class="dropdown-item" id="js-copy-permalink" style="cursor:pointer;">
            Copy permalink
          </clipboard-copy>
        </li>
        <li><a class="dropdown-item js-update-url-with-hash" id="js-view-git-blame" role="menuitem" href="/gabors-data-analysis/da_case_studies/blame/5cbdf3e320c2a27257601507105e42583ab03cb8/ch18-case-shiller-la/ch18-ts-pred-housprices.R">View git blame</a></li>
          <li><a class="dropdown-item" id="js-new-issue" role="menuitem" href="/gabors-data-analysis/da_case_studies/issues/new">Reference in new issue</a></li>
      </ul>
    </details-menu>
  </details>

  </div>

    </div>

  


  <details class="details-reset details-overlay details-overlay-dark" id="jumpto-line-details-dialog">
    <summary data-hotkey="l" aria-label="Jump to line"></summary>
    <details-dialog class="Box Box--overlay d-flex flex-column anim-fade-in fast linejump" aria-label="Jump to line">
      <!-- '"` --><!-- </textarea></xmp> --></option></form><form class="js-jump-to-line-form Box-body d-flex" action="" accept-charset="UTF-8" method="get">
        <input class="form-control flex-auto mr-3 linejump-input js-jump-to-line-field" type="text" placeholder="Jump to line&hellip;" aria-label="Jump to line" autofocus>
        <button type="submit" class="btn" data-close-dialog>Go</button>
</form>    </details-dialog>
  </details>




  </div>
</div>

    </main>
  </div>
  

  </div>

        
<div class="footer container-xl width-full p-responsive" role="contentinfo">
  <div class="position-relative d-flex flex-row-reverse flex-lg-row flex-wrap flex-lg-nowrap flex-justify-center flex-lg-justify-between pt-6 pb-2 mt-6 f6 text-gray border-top border-gray-light ">
    <ul class="list-style-none d-flex flex-wrap col-12 col-lg-5 flex-justify-center flex-lg-justify-between mb-2 mb-lg-0">
      <li class="mr-3 mr-lg-0">&copy; 2020 GitHub, Inc.</li>
        <li class="mr-3 mr-lg-0"><a data-ga-click="Footer, go to terms, text:terms" href="https://github.com/site/terms">Terms</a></li>
        <li class="mr-3 mr-lg-0"><a data-ga-click="Footer, go to privacy, text:privacy" href="https://github.com/site/privacy">Privacy</a></li>
        <li class="mr-3 mr-lg-0"><a data-ga-click="Footer, go to security, text:security" href="https://github.com/security">Security</a></li>
        <li class="mr-3 mr-lg-0"><a href="https://githubstatus.com/" data-ga-click="Footer, go to status, text:status">Status</a></li>
        <li><a data-ga-click="Footer, go to help, text:help" href="https://docs.github.com">Help</a></li>

    </ul>

    <a aria-label="Homepage" title="GitHub" class="footer-octicon d-none d-lg-block mx-lg-4" href="https://github.com">
      <svg height="24" class="octicon octicon-mark-github" viewBox="0 0 16 16" version="1.1" width="24" aria-hidden="true"><path fill-rule="evenodd" d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"></path></svg>
</a>
   <ul class="list-style-none d-flex flex-wrap col-12 col-lg-5 flex-justify-center flex-lg-justify-between mb-2 mb-lg-0">
        <li class="mr-3 mr-lg-0"><a data-ga-click="Footer, go to contact, text:contact" href="https://github.com/contact">Contact GitHub</a></li>
        <li class="mr-3 mr-lg-0"><a href="https://github.com/pricing" data-ga-click="Footer, go to Pricing, text:Pricing">Pricing</a></li>
      <li class="mr-3 mr-lg-0"><a href="https://docs.github.com" data-ga-click="Footer, go to api, text:api">API</a></li>
      <li class="mr-3 mr-lg-0"><a href="https://services.github.com" data-ga-click="Footer, go to training, text:training">Training</a></li>
        <li class="mr-3 mr-lg-0"><a href="https://github.blog" data-ga-click="Footer, go to blog, text:blog">Blog</a></li>
        <li><a data-ga-click="Footer, go to about, text:about" href="https://github.com/about">About</a></li>
    </ul>
  </div>
  <div class="d-flex flex-justify-center pb-6">
    <span class="f6 text-gray-light"></span>
  </div>
</div>



  <div id="ajax-error-message" class="ajax-error-message flash flash-error">
    <svg class="octicon octicon-alert" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M8.22 1.754a.25.25 0 00-.44 0L1.698 13.132a.25.25 0 00.22.368h12.164a.25.25 0 00.22-.368L8.22 1.754zm-1.763-.707c.659-1.234 2.427-1.234 3.086 0l6.082 11.378A1.75 1.75 0 0114.082 15H1.918a1.75 1.75 0 01-1.543-2.575L6.457 1.047zM9 11a1 1 0 11-2 0 1 1 0 012 0zm-.25-5.25a.75.75 0 00-1.5 0v2.5a.75.75 0 001.5 0v-2.5z"></path></svg>
    <button type="button" class="flash-close js-ajax-error-dismiss" aria-label="Dismiss error">
      <svg class="octicon octicon-x" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M3.72 3.72a.75.75 0 011.06 0L8 6.94l3.22-3.22a.75.75 0 111.06 1.06L9.06 8l3.22 3.22a.75.75 0 11-1.06 1.06L8 9.06l-3.22 3.22a.75.75 0 01-1.06-1.06L6.94 8 3.72 4.78a.75.75 0 010-1.06z"></path></svg>
    </button>
    You can’t perform that action at this time.
  </div>


    <script crossorigin="anonymous" async="async" integrity="sha512-bn/3rKJzBl2H64K38R8KaVcT26vKK7BJQC59lwYc+9fjlHzmy0fwh+hzBtsgTdhIi13dxjzNKWhdSN8WTM9qUw==" type="application/javascript" id="js-conditional-compat" data-src="https://github.githubassets.com/assets/compat-bootstrap-6e7ff7ac.js"></script>
    <script crossorigin="anonymous" integrity="sha512-CxjaMepCmi+z0LTeztU2S8qGD25LyHD6j9t0RSPevy63trFWJVwUM6ipAVLgtpMBBgZ53wq8JPkSeQ6ruaZL2w==" type="application/javascript" src="https://github.githubassets.com/assets/environment-bootstrap-0b18da31.js"></script>
    <script crossorigin="anonymous" async="async" integrity="sha512-ZpT71hbwZH9CGy7BM/GLovEPgyslK7oJpsKzHFJv5wMAhVrxsGpXYoF7aMHGrOu3SQZd9cv/HN+2dn2d/FNx/A==" type="application/javascript" src="https://github.githubassets.com/assets/vendor-6694fbd6.js"></script>
    <script crossorigin="anonymous" async="async" integrity="sha512-cvRQ/2/cKbueC6DEML4Z6tgaTYWc2rbEwXFTUvqZls7IizI5TuSOBhua5UgHHOSvEt2mm3KpB6FxuAOEtz+mTg==" type="application/javascript" src="https://github.githubassets.com/assets/frameworks-72f450ff.js"></script>
    
    <script crossorigin="anonymous" async="async" integrity="sha512-asendBl8xOaEqbbOmG2teQ8I2X/O7pS3103VUc5Kv4G+RvxIv/TlUWL8VhxCEXz4g5HSDMRj4T8uCRmbyvb0Og==" type="application/javascript" src="https://github.githubassets.com/assets/behaviors-bootstrap-6ac7a774.js"></script>
    
      <script crossorigin="anonymous" async="async" integrity="sha512-TjmDUfspgN28WRWfc01tOL0BFS8pI/TAi8TQ665TcA3jG1C3QgfFu0YKa32Z03rlEL8dukbsy+amwBzgGyjESQ==" type="application/javascript" data-module-id="./Sortable.js" data-src="https://github.githubassets.com/assets/Sortable-4e398351.js"></script>
      <script crossorigin="anonymous" async="async" integrity="sha512-8hScl0DkWwAjCqAQA50kQOn2QTYfPcKEyJjkKYtjGB88r9GB/6kmBBsneJPgwhW3yewwt64ABgsQGpQSLX8zpg==" type="application/javascript" data-module-id="./contributions-spider-graph.js" data-src="https://github.githubassets.com/assets/contributions-spider-graph-f2149c97.js"></script>
      <script crossorigin="anonymous" async="async" integrity="sha512-QOViDUFlNJwNDkBXlntZ4AUm/OFx3TuM7jg4Z1tb3E7dnf3V5p3Oh3E8cwRdjDMDBp4LyOdoBFIhur0biAIdlw==" type="application/javascript" data-module-id="./drag-drop.js" data-src="https://github.githubassets.com/assets/drag-drop-40e5620d.js"></script>
      <script crossorigin="anonymous" async="async" integrity="sha512-iLuC2weaJqL9mYAud2WDWjhd8cJe8dXVxw2KhCH2Rnj6WJvTzlZRmvTtL09wNWX6nRze/TDaQ7gq7BFLchaDYg==" type="application/javascript" data-module-id="./image-crop-element-loader.js" data-src="https://github.githubassets.com/assets/image-crop-element-loader-88bb82db.js"></script>
      <script crossorigin="anonymous" async="async" integrity="sha512-h6a6OmeoK5aqV+JBs2RH8D1xQ1IS4k67sOi5DUmHMV6dv5xVP4Wj37fKucTbjnvuT1IIO4bC/qfRl+BrCMigjg==" type="application/javascript" data-module-id="./jump-to.js" data-src="https://github.githubassets.com/assets/jump-to-87a6ba3a.js"></script>
      <script crossorigin="anonymous" async="async" integrity="sha512-HzWUeLy0p20M4Lc3+EerTwy/VaH3vMuKLvhFJr0PsJfKXnsD9oy5SfashhxStUirglhYZUB4fLYQRM1uzrFyNg==" type="application/javascript" data-module-id="./profile-pins-element.js" data-src="https://github.githubassets.com/assets/profile-pins-element-1f359478.js"></script>
      <script crossorigin="anonymous" async="async" integrity="sha512-qECv/jhsvLFN77eGNu0cjMR2+zvAlLyhQVTnmayJc5OLZoxMLjQZxZW1hK/dhcYro6Wec/aiF21HYf2N5OilYQ==" type="application/javascript" data-module-id="./randomColor.js" data-src="https://github.githubassets.com/assets/randomColor-a840affe.js"></script>
      <script crossorigin="anonymous" async="async" integrity="sha512-mHqsE5aQq7fAmmLd0epHBJK8rn8DOVnjW2YQOT8wvsN1oLrypw0cDFmwXPDwbMghHyo4kKiOtVJ/kEsEzwwibw==" type="application/javascript" data-module-id="./tweetsodium.js" data-src="https://github.githubassets.com/assets/tweetsodium-987aac13.js"></script>
      <script crossorigin="anonymous" async="async" integrity="sha512-WIOX7irV0ZR7jHpy3hhnUvbOMV6Zfu71QFQaANL5zG1k648Mv703s+V2ohGSEhdwf1VC+Bw/NdgyWIQBt+YJLA==" type="application/javascript" data-module-id="./user-status-submit.js" data-src="https://github.githubassets.com/assets/user-status-submit-588397ee.js"></script>
    
    <script crossorigin="anonymous" async="async" integrity="sha512-Iklsy9YupvgRWe3v9SejEpXhpW3E8+fbRMj46o4nSgLQOmuJbNrYSXBdP08V2fZjf9NGh6ME+7XapmN9UNLcmw==" type="application/javascript" src="https://github.githubassets.com/assets/github-bootstrap-22496ccb.js"></script>
  <div class="js-stale-session-flash flash flash-warn flash-banner" hidden
    >
    <svg class="octicon octicon-alert" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M8.22 1.754a.25.25 0 00-.44 0L1.698 13.132a.25.25 0 00.22.368h12.164a.25.25 0 00.22-.368L8.22 1.754zm-1.763-.707c.659-1.234 2.427-1.234 3.086 0l6.082 11.378A1.75 1.75 0 0114.082 15H1.918a1.75 1.75 0 01-1.543-2.575L6.457 1.047zM9 11a1 1 0 11-2 0 1 1 0 012 0zm-.25-5.25a.75.75 0 00-1.5 0v2.5a.75.75 0 001.5 0v-2.5z"></path></svg>
    <span class="js-stale-session-flash-signed-in" hidden>You signed in with another tab or window. <a href="">Reload</a> to refresh your session.</span>
    <span class="js-stale-session-flash-signed-out" hidden>You signed out in another tab or window. <a href="">Reload</a> to refresh your session.</span>
  </div>
  <template id="site-details-dialog">
  <details class="details-reset details-overlay details-overlay-dark lh-default text-gray-dark hx_rsm" open>
    <summary role="button" aria-label="Close dialog"></summary>
    <details-dialog class="Box Box--overlay d-flex flex-column anim-fade-in fast hx_rsm-dialog hx_rsm-modal">
      <button class="Box-btn-octicon m-0 btn-octicon position-absolute right-0 top-0" type="button" aria-label="Close dialog" data-close-dialog>
        <svg class="octicon octicon-x" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M3.72 3.72a.75.75 0 011.06 0L8 6.94l3.22-3.22a.75.75 0 111.06 1.06L9.06 8l3.22 3.22a.75.75 0 11-1.06 1.06L8 9.06l-3.22 3.22a.75.75 0 01-1.06-1.06L6.94 8 3.72 4.78a.75.75 0 010-1.06z"></path></svg>
      </button>
      <div class="octocat-spinner my-6 js-details-dialog-spinner"></div>
    </details-dialog>
  </details>
</template>

  <div class="Popover js-hovercard-content position-absolute" style="display: none; outline: none;" tabindex="0">
  <div class="Popover-message Popover-message--bottom-left Popover-message--large Box box-shadow-large" style="width:360px;">
  </div>
</div>


  </body>
</html>

