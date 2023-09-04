import request from '@/utils/jqRequest'

// 查询单号自动获取设置列表
export function listNosetting(query) {
  return request({
    url: '/system/nosetting/list',
    method: 'get',
    params: query
  })
}

// 查询单号自动获取设置详细
export function getNosetting(id) {
  return request({
    url: '/system/nosetting/' + id,
    method: 'get'
  })
}

// 新增单号自动获取设置
export function addNosetting(data) {
  return request({
    url: '/system/nosetting',
    method: 'post',
    data: data
  })
}

// 修改单号自动获取设置
export function updateNosetting(data) {
  return request({
    url: '/system/nosetting',
    method: 'put',
    data: data
  })
}

// 删除单号自动获取设置
export function delNosetting(id) {
  return request({
    url: '/system/nosetting/' + id,
    method: 'delete'
  })
}

// 导出单号自动获取设置
export function exportNosetting(query) {
  return request({
    url: '/system/nosetting/export',
    method: 'get',
    params: query
  })
}
