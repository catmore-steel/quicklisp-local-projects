<template>
  <div style="display: flex;width: 100%; height: calc(100vh - 84px);padding: 20px">
    <div class="left_box">
      <div class="top_box">
        <h4>功能手册
          <el-button @click="jumpDoc" type="primary" style="margin-left: 20px;">查看加计扣除辅助系统操作手册
          </el-button>
        </h4>
        <video
          src="http://220.178.166.133:8086/index.php?user/publicLink&fid=5a0c2DIslPW8RE7CtJNabB3zDIB9FdDpqFD-OA1auPO8jNgj-US8C7tGcWN8dccuWD_Bz0GUsfO5G3eSQqJd5KMNtbni6mgfqcIhqBs6wBF-p_nCis3BE0Oka6omqnwDFhb4-g4&file_name=/%E9%AB%98%E4%BC%81%E5%9F%B9%E8%AE%AD%E8%A7%86%E9%A2%91.mp4"
          controls></video>
        <!--        <div class="doc"></div>-->
      </div>
      <div class="bottom_box">
        <h4>使用疑问</h4>
        <div class="question">
        </div>
      </div>
    </div>
    <div class="right_box">
      <h4>更新日志</h4>
      <div class="log_wrap">
        <div v-for="item in updateLog" class="list_wrap">
          <img src="../assets/icons/img/yuanquan.png"/>
          <div class="version" @click="jumpVrsionDetail(item)">V{{ item.versionNo }}{{ cmpDate(item.createTime) }}</div>
          <div class="detail">
            <li v-for="item2 in item.detailList">•
              <span class="type" :style="{
                  backgroundColor: item2.versionType == '1' ? '#00abed' : item2.versionType == '2' ? '#17bc9c' : '#ffb800'
                }">{{ item2.versionTypeName }}</span>
              <span class="info">{{ item2.versionDetail }}</span>
            </li>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import PanelGroup from './dashboard/PanelGroup'
import LineChart from './dashboard/LineChart'
import RaddarChart from './dashboard/RaddarChart'
import PieChart from './dashboard/PieChart'
import BarChart from './dashboard/BarChart'
import { updateLog } from '@/api/system/home'

const lineChartData = {
  newVisitis: {
    expectedData: [100, 120, 161, 134, 105, 160, 165],
    actualData: [120, 82, 91, 154, 162, 140, 145]
  },
  messages: {
    expectedData: [200, 192, 120, 144, 160, 130, 140],
    actualData: [180, 160, 151, 106, 145, 150, 130]
  },
  purchases: {
    expectedData: [80, 100, 121, 104, 105, 90, 100],
    actualData: [120, 90, 100, 138, 142, 130, 130]
  },
  shoppings: {
    expectedData: [130, 140, 141, 142, 145, 150, 160],
    actualData: [120, 82, 91, 154, 162, 140, 130]
  }
}

export default {
  name: 'Index',
  components: {
    PanelGroup,
    LineChart,
    RaddarChart,
    PieChart,
    BarChart
  },
  data() {
    return {
      waitDraftNum: null,
      waitReplyNum: null,
      warnDig: true,
      lineChartData: lineChartData.newVisitis,
      updateLog: []
    }
  },
  created() {
    this.getUpdateLog()
  },
  methods: {
    cmpDate(date) {
      const farmeDate = new Date(date)
      var year = farmeDate.getFullYear()
      var month = farmeDate.getMonth() + 1 < 10 ? '0' + (farmeDate.getMonth() + 1) : farmeDate.getMonth() + 1
      var day = farmeDate.getDate() < 10 ? '0' + farmeDate.getDate() : farmeDate.getDate()
      return `_${year}${month}${day}`
    },
    jumpDoc() {
      window.open('https://doc.jqkjw.com/doc/3/', '_blank')
    },
    jumpVrsionDetail(item) {
      item.versionDetail && window.open(item.versionDetail, '_blank')
    },
    handleSetLineChartData(type) {
      this.lineChartData = lineChartData[type]
    },
    async getUpdateLog() {
      const data = await updateLog('aic')
      if (data && data.code == 200) {
        this.updateLog = data.data.reverse()
      }
    }
  }
}
</script>

<style lang="scss" scoped>
.top_box, .bottom_box, .right_box {
  border: 2px solid #eee;
  border-radius: 5px;
  padding: 20px;

  h4 {
    text-align: left;
    margin: 0;
    color: #333;
  }
}

.left_box {
  width: calc(70% - 20px);
  margin-right: 20px;
  height: 100%;

  .top_box {
    height: calc(50% - 10px);
    margin-bottom: 20px;
    text-align: center;

    video {
      height: calc(100% - 40px);
      padding-top: 20px;
      //height: 100%;
    }

    .doc {
      margin: 10px 0;
      width: 100%;
      height: calc(100% - 20px);
      background-color: #eee;
    }
  }

  .bottom_box {
    height: calc(50% - 10px);

    .question {
      margin: 10px 0;
      width: 100%;
      height: calc(100% - 20px);
      background-color: #eee;
    }
  }
}

.right_box {
  width: 30%;
  height: 100%;
  overflow-y: auto;

  .log_wrap {
    margin-left: 20px;
    margin-top: 20px;
    padding-left: 10px;
    border-left: 2px solid #ccc;

    .list_wrap {
      position: relative;
      padding-left: 10px;

      img {
        background-color: #fff;
        width: 22px;
        height: 22px;
        position: absolute;
        top: -2px;
        left: -22px;
      }

      .version {
        cursor: pointer;
        font-size: 17px;
        color: #333;
        font-weight: bold;
      }

      .detail {
        font-size: 14px;
        margin: 15px 0 15px 0;

        li {
          list-style-type: none;
          min-height: 28px;
          line-height: 28px;
          color: #999;
        }

        .type {
          margin: 0 10px;
          padding: 3px 10px;
          border-radius: 5px;
          background-color: #17bc9c;
          color: #fff;
        }

        .info {
        }
      }
    }
  }
}

.dashboard-editor-container {
  padding: 32px;
  background-color: rgb(240, 242, 245);
  position: relative;
  font-style: normal;
  font-variant-ligatures: normal;
  font-variant-caps: normal;
  font-variant-numeric: normal;
  font-variant-east-asian: normal;
  font-weight: 400;
  font-stretch: normal;
  font-size: 13.3333px;
  line-height: normal;
  font-family: Arial;
  min-height: calc(100vh - 84px);

  .chart-wrapper {
    background: #fff;
    padding: 16px 16px 0;
    margin-bottom: 32px;
  }
}

.main-container {
  background-color: #f0f2f5 !important;
}

@media (max-width: 1024px) {
  .chart-wrapper {
    padding: 8px;
  }
}


</style>
