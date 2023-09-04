<template>
  <div>
    <el-row><i class="iconfont jq-ico-shuxian"></i> 专利数据统计</el-row>
    <el-row :gutter="40" class="panel-group">
      <el-col :xs="12" :sm="12" :lg="6" class="card-panel-col">
        <div class="card-panel" @click="handleSetLineChartData('newVisitis')">
          <div class="card-panel-icon-wrapper icon-people">
            <!--          <svg-icon icon-class="peoples" class-name="card-panel-icon" />-->
            <img class="" src="../../assets/workbench/image/patent01.png"
                 style="height: 80px;width: 80px;margin-bottom: 10px">
          </div>
          <div class="card-panel-description">
            <div class="card-panel-text">
              提案总数
            </div>
            <count-to :start-val="0" :end-val="form.newCase" :duration="1000" class="card-panel-num"/>
          </div>
        </div>
      </el-col>
      <el-col :xs="12" :sm="12" :lg="6" class="card-panel-col">
        <div class="card-panel" @click="handleSetLineChartData('messages')">
          <div class="card-panel-icon-wrapper icon-message">
            <img class="" src="../../assets/workbench/image/patent02.png"
                 style="height: 80px;width: 80px;margin-bottom: 10px">
          </div>
          <div class="card-panel-description">
            <div class="card-panel-text">
              案件总数
            </div>
            <count-to :start-val="0" :end-val="form.allCase" :duration="1000" class="card-panel-num"/>
          </div>
        </div>
      </el-col>
      <el-col :xs="12" :sm="12" :lg="6" class="card-panel-col">
        <div class="card-panel" @click="handleSetLineChartData('purchases')">
          <div class="card-panel-icon-wrapper icon-money">
            <img class="" src="../../assets/workbench/image/patent03.png"
                 style="height: 80px;width: 80px;margin-bottom: 10px">
          </div>
          <div class="card-panel-description">
            <div class="card-panel-text">
              已授权专利
            </div>
            <count-to :start-val="0" :end-val="form.grantPatent" :duration="1000" class="card-panel-num"/>
          </div>
        </div>
      </el-col>
      <el-col :xs="12" :sm="12" :lg="6" class="card-panel-col">
        <div class="card-panel" @click="handleSetLineChartData('shoppings')">
          <div class="card-panel-icon-wrapper icon-shopping">
            <img class="" src="../../assets/workbench/image/patent04.png"
                 style="height: 80px;width: 80px;margin-bottom: 10px">
          </div>
          <div class="card-panel-description">
            <div class="card-panel-text">
              专利总数
            </div>
            <count-to :start-val="0" :end-val="form.allPatent" :duration="1000" class="card-panel-num"/>
          </div>
        </div>
      </el-col>
    </el-row>
  </div>
</template>

<script>
  import CountTo from 'vue-count-to'

  export default {
    components: { CountTo },
    data(){
      return {
        form: {
          newCase: 0,
          allCase: 0,
          grantPatent: 0,
          allPatent: 0,
        },
      }
    },
    created() {
    },
    methods: {
      handleSetLineChartData(type) {
        this.$emit('handleSetLineChartData', type)
      }
    }
  }
</script>

<style lang="scss" scoped>
  .panel-group {
    margin-top: 18px;

    .card-panel-col {
      margin-bottom: 32px;
    }

    .card-panel {
      height: 108px;
      cursor: pointer;
      font-size: 12px;
      position: relative;
      overflow: hidden;
      color: #666;
      background: #fff;
      box-shadow: 4px 4px 40px rgba(0, 0, 0, .05);
      border-color: rgba(0, 0, 0, .05);

      &:hover {
        .card-panel-icon-wrapper {
          color: #fff;
        }

        .icon-people {
          background: #40c9c6;
        }

        .icon-message {
          background: #36a3f7;
        }

        .icon-money {
          background: #f4516c;
        }

        .icon-shopping {
          background: #34bfa3
        }
      }

      .icon-people {
        color: #40c9c6;
      }

      .icon-message {
        color: #36a3f7;
      }

      .icon-money {
        color: #f4516c;
      }

      .icon-shopping {
        color: #34bfa3
      }

      .card-panel-icon-wrapper {
        float: left;
        margin: 14px 0 0 14px;
        padding: 16px;
        transition: all 0.38s ease-out;
        border-radius: 6px;
      }

      .card-panel-icon {
        float: left;
        font-size: 48px;
      }

      .card-panel-description {
        margin-left: 120px;
        margin-top: 26px;
        margin-bottom: 26px;
        font-weight: bold;

        .card-panel-text {
          line-height: 18px;
          color: rgba(0, 0, 0, 0.45);
          font-size: 16px;
          margin-bottom: 12px;
        }

        .card-panel-num {
          font-size: 20px;
        }
      }
    }
  }

  .panel-group .card-panel .card-panel-icon-wrapper {
    margin: 0px 0px 0px 0px !important;
    padding: 10px !important;
  }

  @media (max-width: 550px) {
    .card-panel-description {
      display: none;
    }

    .card-panel-icon-wrapper {
      float: none !important;
      width: 100%;
      height: 100%;
      margin: 0 !important;

      .svg-icon {
        display: block;
        margin: 14px auto !important;
        float: none !important;
      }
    }
  }

  .jq-ico-shuxian{
    color: rgb(24, 144, 255);
  }
</style>
