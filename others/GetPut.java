public class GetPut<T>
{
  private T _value;

  public T Get()
  {
    return _value;
  }

  public T Put(T x)
  {
    T old = _value;
    _value = x;
    return old;
  }

}

