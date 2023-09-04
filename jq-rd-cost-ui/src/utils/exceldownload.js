import { getToken } from '@/utils/auth'
import axios from 'axios'
import { Message, Loading } from 'element-ui'

function excelDownLoad(url, param, e,className) {
  let loadingInstance = null
  if (e != undefined) {
    let target = e.currentTarget.parentElement
    while (target.className != className) {
      target = target.parentElement
    }
    loadingInstance = Loading.service({
      target: target.parentElement,
      background: '#a6d6ff57',
      text: '正在执行数据导出...'
    })
  }
  axios({
    method: 'get',
    params: param,
    url: process.env.VUE_APP_BASE_API + url,
    responseType: 'blob',
    headers: { 'Authorization': 'Bearer ' + getToken() }
  }).then(res => {
    loadingInstance != null ?loadingInstance.close() :{}
    var filename = res.headers['filename']
    // 文件下载
    const blob = new Blob([res.data], {
      type: 'application/vnd.ms-excel'
    })
    let link = document.createElement('a')
    link.setAttribute('download', decodeURI(filename))
    link.href = URL.createObjectURL(blob)
    link.click()
  }).catch(e => {
    loadingInstance != null ?loadingInstance.close() :{}
    Message({
      message: e.message,
      duration: 6000,
      showClose: true,
      type: 'error'
    })
  })
}

export default excelDownLoad
