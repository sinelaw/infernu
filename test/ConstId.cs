class Test
{
  public Func<T,T> ConstId<U>(U a)
  {
    return x => x;
  }
}