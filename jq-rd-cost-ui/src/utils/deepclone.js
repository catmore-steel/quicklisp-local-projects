/**
 * 对象深拷贝
 */
export const deepClone = data => {
  const type = Object.prototype.toString.call(data)
  let obj
  if (type === 'array') {
    obj = []
  } else if (type === 'object') {
    obj = {}
  } else {
    // 不再具有下一层次
    return data
  }
  if (type === 'array') {
    let i = 0, len = data.length
    for (; i < len; i++) {
      obj.push(deepClone(data[i]))
    }
  } else if (type === 'object') {
    for (const item in data) {
      obj[item] = deepClone(data[item])
    }
  }
  return obj
}
