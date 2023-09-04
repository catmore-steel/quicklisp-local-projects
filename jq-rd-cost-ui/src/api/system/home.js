// 主页的接口

import request from '@/utils/jqRequest'

export function updateLog(productCode) {
  return request({
    url: '/highCloud/openAi/listVersionByApi?productCode=' + productCode,
    method: 'get'
  })
}
