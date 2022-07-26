# Twitter Scrapers

I ran multiple twitter scrapers, mainly because I hadn't thought through the most efficient way to do this. I might create one cohesive, more efficient scaper at a later point.
For now, tweet_scraper scrapes the latest 1000 tweets, tweet_scraper_hyperactive scrapes the latest 3000 tweets of those users whose latest 1000 tweets did not include the tweets relevant to the specified timespan (one year before deal date).
pfp_network scrapes users' profile picture url as well as follower count. It also includes code for downloading the profile pictures as jpg.
