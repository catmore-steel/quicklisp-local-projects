import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询研发阶段直接投入明细详细
export function getStageMaterial(id) {
  return request({
    url: '/cost/costStageMaterial/' + id,
    method: 'get'
  })
}

// 新增研发阶段直接投入明细
export function addStageMaterial(data) {
  return request({
    url: '/cost/costStageMaterial',
    method: 'post',
    data: data
  })
}

// 修改研发阶段直接投入明细
export function updateStageMaterial(data) {
  return request({
    url: '/cost/costStageMaterial',
    method: 'put',
    data: data
  })
}

// 删除研发阶段直接投入明细
export function delStageMaterial(id) {
  return request({
    url: '/cost/costStageMaterial/' + id,
    method: 'delete'
  })
}

// 导出研发阶段直接投入明细
export function exportStageMaterial(query) {
  return excelDownLoad('/cost/costStageMaterial/export',query)
}
