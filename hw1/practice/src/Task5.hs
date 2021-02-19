module Task5 where

data Post = Post { pTitle :: String, pBody :: String }

data Blog = Blog
  { bPosts   :: [Post]
  , bCounter :: Int
  }

data BlogM a = BlogM { runBlogM :: Blog -> (a, Blog) }

instance Functor BlogM where
  fmap f mb = do
    b <- mb
    return (f b)

instance Applicative BlogM where
  pure = return
  mf <*> mb = do
    f <- mf
    b <- mb
    return (f b)

instance Monad BlogM where
  return x = BlogM $ \b -> (x, b)
  m >>= k = BlogM $ \blog ->
    case runBlogM m blog of
      (a, newBlog) -> runBlogM (k a) newBlog

readPost :: Int -> BlogM Post
readPost n = BlogM $ \b -> (bPosts b !! n, b)

newPost :: Post -> BlogM ()
newPost p = BlogM $ \b -> ((), Blog (p : bPosts b) (bCounter b + 1))