import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'
import jqRequest from '@/utils/jqRequest'

export function listAccountConfig(query) {
  return jqRequest({
    url: '/conf/confAccountConfig/listAccountConfig',
    method: 'get',
    params: query
  })
}

// 查询会计科目配置详细
export function getConfAccountConfig(id) {
  return request({
    url: '/conf/confAccountConfig/' + id,
    method: 'get'
  })
}

// 新增会计科目配置
export function addConfAccountConfig(data) {
  return request({
    url: '/conf/confAccountConfig',
    method: 'post',
    data: data
  })
}

// 修改会计科目配置
export function updateConfAccountConfig(data) {
  return request({
    url: '/conf/confAccountConfig',
    method: 'put',
    data: data
  })
}

// 删除会计科目配置
export function delConfAccountConfig(id) {
  return request({
    url: '/conf/confAccountConfig/' + id,
    method: 'delete'
  })
}

// 导出会计科目配置
export function exportConfAccountConfig(query) {
  return excelDownLoad('/conf/confAccountConfig/export',query)
}
