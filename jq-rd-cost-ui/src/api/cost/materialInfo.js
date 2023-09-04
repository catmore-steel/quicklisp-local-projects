import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询企业物料信息详细
export function getMaterialInfo(id) {
  return request({
    url: '/cost/materialInfo/' + id,
    method: 'get'
  })
}

// 新增企业物料信息
export function addMaterialInfo(data) {
  return request({
    url: '/cost/materialInfo',
    method: 'post',
    data: data
  })
}

// 修改企业物料信息
export function updateMaterialInfo(data) {
  return request({
    url: '/cost/materialInfo',
    method: 'put',
    data: data
  })
}

// 删除企业物料信息
export function delMaterialInfo(id) {
  return request({
    url: '/cost/materialInfo/' + id,
    method: 'delete'
  })
}

// 导出企业物料信息
export function exportMaterialInfo(query) {
  return excelDownLoad('/cost/materialInfo/export',query)
}

// 下载物料导入模板
export function importTemplate() {
  return request({
    url: '/cost/materialInfo/importTemplate',
    method: 'get'
  })
}
