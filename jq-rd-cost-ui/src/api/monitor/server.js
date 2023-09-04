import request from '@/utils/jqRequest'

// 查询服务器详细
export function getServer() {
  return request({
    url: '/monitor/server',
    method: 'get'
  })
}
