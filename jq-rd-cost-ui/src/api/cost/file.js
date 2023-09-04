import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询汇总文件详细
export function getFile(id) {
  return request({
    url: '/cost/file/' + id,
    method: 'get'
  })
}

// 新增汇总文件
export function addFile(data) {
  return request({
    url: '/cost/file',
    method: 'post',
    data: data
  })
}

// 修改汇总文件
export function updateFile(data) {
  return request({
    url: '/cost/file',
    method: 'put',
    data: data
  })
}

// 删除汇总文件
export function delFile(id) {
  return request({
    url: '/cost/file/' + id,
    method: 'delete'
  })
}

// 导出汇总文件
export function exportFile(query) {
  return excelDownLoad('/cost/file/export',query)
}
