// DOM Elements
const questionTextElement = document.getElementById('question-text');
const optionsContainerElement = document.getElementById('options-container');
const resultContainerElement = document.getElementById('result-container');
const testNameElement = document.getElementById('test-name');
const testDescriptionElement = document.getElementById('test-description');
const testWhenToUseElement = document.getElementById('test-when-to-use');
const rCodeElement = document.getElementById('r-code');
const rCodeNotesElement = document.getElementById('r-code-notes');
const resetButtonElement = document.getElementById('reset-button');
const questionContainerElement = document.getElementById('question-container');
const copyCodeButtonElement = document.getElementById('copy-code-button');
const copyButtonTextElement = document.getElementById('copy-button-text');

const userPathContainerElement = document.getElementById('user-path-container');
const userPathListElement = document.getElementById('user-path-list');

// --- Decision Tree Data Structure ---
// بازسازی شده بر اساس https://stats.oarc.ucla.edu/other/mult-pkg/whatstat/
const decisionTree = {
    start: {
        question: "نوع متغیر وابسته اصلی (Dependent Variable) شما چیست؟",
        options: [
            { text: "پیوسته (Interval / Ratio)", nextNode: "q_dv_continuous_num_dvs" },
            { text: "طبقه‌ای / دسته‌ای (Categorical: Nominal / Ordinal)", nextNode: "q_dv_categorical_num_dvs" },
            { text: "شمارشی (Count)", nextNode: "q_dv_count_num_dvs" }
        ]
    },

    // --- مسیر متغیر وابسته پیوسته ---
    q_dv_continuous_num_dvs: {
        question: "چند متغیر وابسته پیوسته دارید؟",
        options: [
            { text: "یک متغیر وابسته پیوسته", nextNode: "q_one_dv_continuous_num_ivs" },
            { text: "دو یا چند متغیر وابسته پیوسته", nextNode: "q_multi_dv_continuous_num_ivs" }
        ]
    },

    // --- یک متغیر وابسته پیوسته ---
    q_one_dv_continuous_num_ivs: {
        question: "چند متغیر مستقل (Independent Variable) اصلی دارید؟",
        options: [
            { text: "هیچ (مقایسه میانگین با یک مقدار ثابت یا بررسی توزیع)", nextNode: "q_one_dv_continuous_no_iv_goal" },
            { text: "یک متغیر مستقل", nextNode: "q_one_dv_one_iv_type" },
            { text: "دو یا چند متغیر مستقل", nextNode: "q_one_dv_multi_iv_type" }
        ]
    },
    q_one_dv_continuous_no_iv_goal: {
        question: "هدف شما چیست؟",
        options: [
            { text: "مقایسه میانگین نمونه با یک مقدار مشخص/فرضی", nextNode: "test_one_sample_t_test" },
            { text: "بررسی نرمال بودن توزیع متغیر", nextNode: "test_shapiro_wilk" }
        ]
    },
    q_one_dv_one_iv_type: {
        question: "نوع متغیر مستقل شما چیست؟",
        options: [
            { text: "طبقه‌ای با دو سطح/گروه (مثلاً زن/مرد، درمان/کنترل)", nextNode: "q_one_dv_one_iv_cat2_paired" },
            { text: "طبقه‌ای با سه یا چند سطح/گروه (مثلاً سه روش درمانی مختلف)", nextNode: "q_one_dv_one_iv_cat_multi_paired" },
            { text: "پیوسته (Interval / Ratio)", nextNode: "test_simple_linear_regression_or_correlation" },
            { text: "ترتیبی (Ordinal)", nextNode: "q_one_dv_one_iv_ordinal_iv_options" }
        ]
    },
    q_one_dv_one_iv_ordinal_iv_options: {
        question: "متغیر مستقل ترتیبی شما چند سطح دارد و هدف چیست؟",
        options: [
            { text: "دو سطح ترتیبی (معادل طبقه‌ای دو سطحی)", nextNode: "q_one_dv_one_iv_cat2_paired"}, // Treat as dichotomous categorical
            { text: "بیش از دو سطح ترتیبی، بررسی همبستگی", nextNode: "test_spearman_correlation"}, // Spearman or Kendall's Tau
            { text: "بیش از دو سطح ترتیبی، مقایسه گروه‌ها (معادل طبقه‌ای چند سطحی)", nextNode: "q_one_dv_one_iv_cat_multi_paired_nonparametric"} // Non-parametric ANOVA (Kruskal-Wallis)
        ]
    },
    test_simple_linear_regression_or_correlation: {
        question: "آیا هدف پیش‌بینی/مدل‌سازی رابطه (رگرسیون) است یا فقط سنجش میزان و جهت همبستگی (کورولیشن)؟",
        options: [
            { text: "پیش‌بینی / مدل‌سازی رابطه (رگرسیون خطی ساده)", nextNode: "test_simple_linear_regression" },
            { text: "سنجش میزان و جهت همبستگی (کورولیشن پیرسون)", nextNode: "test_pearson_correlation" }
        ]
    },
    q_one_dv_one_iv_cat2_paired: {
        question: "آیا مشاهدات/گروه‌ها مستقل هستند یا جفت شده/وابسته (repeated measures)؟",
        options: [
            { text: "مستقل (Independent samples)", nextNode: "q_one_dv_one_iv_cat2_ind_normality" },
            { text: "جفت شده / وابسته (Paired samples / Repeated measures)", nextNode: "q_one_dv_one_iv_cat2_paired_normality" }
        ]
    },
    q_one_dv_one_iv_cat2_ind_normality: {
        question: "آیا پیش‌فرض نرمال بودن داده‌ها و همگنی واریانس‌ها برقرار است؟",
        options: [
            { text: "بله، یا حجم نمونه بزرگ است.", nextNode: "test_independent_samples_t_test" },
            { text: "خیر، یا مطمئن نیستم (استفاده از آزمون ناپارامتریک).", nextNode: "test_mann_whitney_u" }
        ]
    },
    q_one_dv_one_iv_cat2_paired_normality: {
        question: "آیا پیش‌فرض نرمال بودن تفاوت‌های جفت‌شده برقرار است؟",
        options: [
            { text: "بله، یا حجم نمونه بزرگ است.", nextNode: "test_paired_samples_t_test" },
            { text: "خیر، یا مطمئن نیستم (استفاده از آزمون ناپارامتریک).", nextNode: "test_wilcoxon_signed_rank_paired" }
        ]
    },
    q_one_dv_one_iv_cat_multi_paired: { // For 3+ groups, parametric path
        question: "آیا مشاهدات/گروه‌ها مستقل هستند یا جفت شده/وابسته (repeated measures)؟ (با فرض نرمالیتی و همگنی واریانس‌ها)",
        options: [
            { text: "مستقل", nextNode: "test_one_way_anova" },
            { text: "جفت شده / وابسته", nextNode: "test_one_way_repeated_measures_anova" }
        ]
    },
    q_one_dv_one_iv_cat_multi_paired_nonparametric: { // For 3+ groups, non-parametric path
        question: "آیا مشاهدات/گروه‌ها مستقل هستند یا جفت شده/وابسته (repeated measures)؟ (برای آزمون‌های ناپارامتریک)",
        options: [
            { text: "مستقل", nextNode: "test_kruskal_wallis" },
            { text: "جفت شده / وابسته", nextNode: "test_friedman_test" }
        ]
    },
    q_one_dv_multi_iv_type: {
        question: "ماهیت متغیرهای مستقل شما چگونه است؟",
        options: [
            { text: "همگی طبقه‌ای (Categorical)", nextNode: "q_one_dv_multi_iv_all_cat_interaction" },
            { text: "همگی پیوسته (Continuous)", nextNode: "test_multiple_regression" },
            { text: "ترکیبی از پیوسته و طبقه‌ای", nextNode: "test_ancova_or_regression_with_dummies" }
        ]
    },
    q_one_dv_multi_iv_all_cat_interaction: {
        question: "آیا علاقه‌مند به بررسی اثرات متقابل (interaction effects) بین متغیرهای مستقل طبقه‌ای هستید؟",
        options: [
            { text: "بله، می‌خواهم اثرات متقابل را بررسی کنم", nextNode: "q_one_dv_multi_iv_all_cat_paired_interaction" },
            { text: "خیر، فقط اثرات اصلی (main effects) مد نظر است", nextNode: "q_one_dv_multi_iv_all_cat_paired_no_interaction" }
        ]
    },
    q_one_dv_multi_iv_all_cat_paired_interaction: {
        question: "آیا داده‌ها شامل اندازه‌گیری‌های مکرر (repeated measures) روی حداقل یکی از فاکتورها هستند؟",
        options: [
            { text: "خیر, تمام فاکتورها بین آزمودنی (between-subjects) هستند.", nextNode: "test_factorial_anova_independent" },
            { text: "بله, حداقل یک فاکتور درون آزمودنی (within-subjects) است.", nextNode: "test_factorial_anova_repeated_measures" }, // Simplified, could be mixed
            { text: "طرح مختلط (Mixed design): هم فاکتور بین آزمودنی و هم درون آزمودنی دارم.", nextNode: "test_mixed_anova" }
        ]
    },
     q_one_dv_multi_iv_all_cat_paired_no_interaction: {
        question: "آیا داده‌ها شامل اندازه‌گیری‌های مکرر (repeated measures) روی حداقل یکی از فاکتورها هستند؟ (فقط اثرات اصلی)",
        options: [
            { text: "خیر, تمام فاکتورها بین آزمودنی هستند.", nextNode: "test_anova_main_effects_only_independent" },
            { text: "بله, حداقل یک فاکتور درون آزمودنی است.", nextNode: "test_repeated_measures_anova_main_effects_only" }
        ]
    },

    // --- دو یا چند متغیر وابسته پیوسته ---
    q_multi_dv_continuous_num_ivs: {
        question: "چند متغیر مستقل (Independent Variable) اصلی دارید و نوع آن‌ها چیست؟",
        options: [
            { text: "یک متغیر مستقل طبقه‌ای", nextNode: "test_manova" },
            { text: "یک متغیر مستقل پیوسته", nextNode: "test_canonical_correlation_or_multivariate_regression_one_iv" }, // If IV is one, it's essentially set of regressions unless IV also multivariate
            { text: "دو یا چند متغیر مستقل طبقه‌ای", nextNode: "test_factorial_manova" },
            { text: "دو یا چند متغیر مستقل پیوسته (یا ترکیبی)", nextNode: "test_multivariate_multiple_regression" }
        ]
    },
    test_canonical_correlation_or_multivariate_regression_one_iv: {
        question: "آیا به بررسی رابطه بین دو مجموعه از متغیرها (یک مجموعه از DVها و یک مجموعه از IVها که اینجا فقط یک IV است) علاقه‌مندید، یا مدل‌سازی هر DV توسط آن IV؟",
        options: [
            { text: "مدل‌سازی هر DV توسط IV (مجموعه‌ای از رگرسیون‌های ساده)", nextNode: "info_set_of_simple_regressions" },
            { text: "بررسی رابطه بین مجموعه DVها و IV (کورولیشن کانونی - کمتر رایج با یک IV)", nextNode: "test_canonical_correlation" }
        ]
    },

    // --- مسیر متغیر وابسته طبقه‌ای ---
    q_dv_categorical_num_dvs: {
        question: "متغیر وابسته طبقه‌ای شما چند سطحی است (دو سطحی یا چند سطحی) و چند متغیر وابسته طبقه‌ای دارید؟",
        options: [
            { text: "یک متغیر وابسته دوتایی (Binary)", nextNode: "q_one_dv_binary_num_ivs" },
            { text: "یک متغیر وابسته چند سطحی اسمی (Nominal)", nextNode: "q_one_dv_multinomial_num_ivs" },
            { text: "یک متغیر وابسته چند سطحی ترتیبی (Ordinal)", nextNode: "q_one_dv_ordinal_num_ivs" },
            { text: "دو یا چند متغیر وابسته طبقه‌ای", nextNode: "test_log_linear_model_or_mca" }
        ]
    },

    // یک متغیر وابسته دوتایی (Binary)
    q_one_dv_binary_num_ivs: {
        question: "چند متغیر مستقل دارید و نوع آن‌ها چیست؟ (برای DV دوتایی)",
        options: [
            { text: "هیچ (بررسی نسبت‌ها یا نیکویی برازش)", nextNode: "q_one_dv_binary_no_iv_goal" },
            { text: "یک متغیر مستقل طبقه‌ای (دو یا چند سطحی)", nextNode: "q_one_dv_binary_one_iv_cat_paired" },
            { text: "یک متغیر مستقل پیوسته یا ترتیبی", nextNode: "test_logistic_regression_simple" }, // Ordinal IV often treated as continuous or dummy in logistic
            { text: "دو یا چند متغیر مستقل (از هر نوع)", nextNode: "test_multiple_logistic_regression" }
        ]
    },
    q_one_dv_binary_no_iv_goal: {
        question: "هدف شما برای متغیر وابسته دوتایی بدون متغیر مستقل چیست؟",
        options: [
            { text: "مقایسه نسبت مشاهده شده با یک نسبت مورد انتظار (نیکویی برازش)", nextNode: "test_binomial_test_or_chi_sq_gof_binary" },
            { text: "توصیف فراوانی‌ها و نسبت‌ها", nextNode: "info_descriptive_frequencies_binary" }
        ]
    },
    q_one_dv_binary_one_iv_cat_paired: {
        question: "آیا مشاهدات/گروه‌ها برای متغیر مستقل طبقه‌ای، مستقل هستند یا جفت شده/وابسته؟",
        options: [
            { text: "مستقل", nextNode: "test_chi_square_test_of_independence_or_fisher" }, // Or Fisher's for small N
            { text: "جفت شده / وابسته (معمولاً برای DV دوتایی که دو بار اندازه‌گیری شده)", nextNode: "test_mcnemar_test" }
        ]
    },

    // یک متغیر وابسته چند سطحی اسمی (Nominal)
    q_one_dv_multinomial_num_ivs: {
        question: "چند متغیر مستقل دارید؟ (برای DV چند سطحی اسمی)",
        options: [
            { text: "هیچ (نیکویی برازش برای چند سطح)", nextNode: "test_chi_square_goodness_of_fit" }, // General GOF
            { text: "یک یا چند متغیر مستقل (از هر نوع)", nextNode: "test_multinomial_logistic_regression" }
        ]
    },

    // یک متغیر وابسته چند سطحی ترتیبی (Ordinal)
    q_one_dv_ordinal_num_ivs: {
        question: "چند متغیر مستقل دارید و نوع آن‌ها چیست؟ (برای DV چند سطحی ترتیبی)",
        options: [
            { text: "هیچ (بررسی توزیع یا مقایسه با توزیع نظری)", nextNode: "info_descriptive_ordinal_or_gof" },
            { text: "یک متغیر مستقل طبقه‌ای", nextNode: "test_mann_whitney_or_kruskal_wallis_for_ordinal_dv" }, // Depending on number of groups in IV
            { text: "یک یا چند متغیر مستقل (از هر نوع، برای مدل‌سازی)", nextNode: "test_ordinal_logistic_regression" }
        ]
    },

    // --- مسیر متغیر وابسته شمارشی (Count) ---
    q_dv_count_num_dvs: { // Assuming one count DV
        question: "چند متغیر مستقل (Independent Variable) اصلی دارید؟ (برای DV شمارشی)",
        options: [
            { text: "هیچ (مقایسه میانگین شمارش با یک مقدار ثابت یا بررسی توزیع)", nextNode: "q_one_dv_count_no_iv_goal" },
            { text: "یک یا چند متغیر مستقل (برای مدل‌سازی نرخ شمارش)", nextNode: "q_one_dv_count_iv_exposure" }
        ]
    },
    q_one_dv_count_no_iv_goal: {
        question: "هدف شما برای متغیر وابسته شمارشی بدون متغیر مستقل چیست؟",
        options: [
            { text: "مقایسه نرخ شمارش مشاهده شده با یک نرخ مورد انتظار (نیکویی برازش پواسون)", nextNode: "test_poisson_goodness_of_fit" },
            { text: "توصیف توزیع شمارش", nextNode: "info_descriptive_count" }
        ]
    },
    q_one_dv_count_iv_exposure: {
        question: "آیا متغیر جبرانی (exposure variable) یا دوره زمانی مشاهده برای هر واحد یکسان است یا متفاوت و می‌خواهید آن را در مدل لحاظ کنید (offset)؟",
        options: [
            { text: "یکسان است یا نمی‌خواهم از offset استفاده کنم.", nextNode: "q_one_dv_count_iv_dispersion" },
            { text: "متفاوت است و می‌خواهم از offset استفاده کنم.", nextNode: "q_one_dv_count_iv_dispersion_offset" }
        ]
    },
    q_one_dv_count_iv_dispersion: {
        question: "آیا پراکندگی بیش از حد (Overdispersion: واریانس بزرگتر از میانگین) در داده‌های شمارشی شما محتمل است؟",
        options: [
            { text: "خیر، یا مطمئن نیستم (شروع با پواسون).", nextNode: "test_poisson_regression" },
            { text: "بله، پراکندگی بیش از حد وجود دارد یا محتمل است.", nextNode: "test_negative_binomial_regression" },
            { text: "داده‌ها شامل تعداد زیادی صفر بیش از حد انتظار هستند (Zero-inflated).", nextNode: "test_zero_inflated_poisson_or_nb" }
        ]
    },
    q_one_dv_count_iv_dispersion_offset: {
        question: "آیا پراکندگی بیش از حد (Overdispersion) در داده‌های شمارشی شما (با لحاظ offset) محتمل است؟",
        options: [
            { text: "خیر، یا مطمئن نیستم (شروع با پواسون با offset).", nextNode: "test_poisson_regression_offset" },
            { text: "بله، پراکندگی بیش از حد وجود دارد یا محتمل است (با offset).", nextNode: "test_negative_binomial_regression_offset" },
            { text: "داده‌ها با offset، شامل تعداد زیادی صفر بیش از حد انتظار هستند.", nextNode: "test_zero_inflated_poisson_or_nb_offset" } // Need specific offset versions for ZI models
        ]
    },

    // --- تعریف آزمون‌های نهایی (Final Tests) و گره‌های اطلاعاتی (info_...) ---
    // ## آزمون‌های مربوط به یک متغیر وابسته پیوسته ##
    test_one_sample_t_test: { /* ... کد از پیام قبلی ... */ },
    test_shapiro_wilk: { /* ... کد از پیام قبلی ... */ },
    test_independent_samples_t_test: { /* ... کد از پیام قبلی ... */ },
    test_paired_samples_t_test: { /* ... کد از پیام قبلی ... */ },
    test_simple_linear_regression: { /* ... کد از پیام قبلی ... */ },
    test_pearson_correlation: {
        isFinal: true,
        name: "همبستگی پیرسون (Pearson Correlation)",
        description: "برای سنجش قدرت و جهت رابطه خطی بین دو متغیر پیوسته.",
        whenToUse: "زمانی که دو متغیر پیوسته دارید و می‌خواهید بدانید چگونه با هم تغییر می‌کنند. پیش‌فرض‌ها شامل نرمال بودن توزیع هر دو متغیر و رابطه خطی است.",
        r_code: `
# data$variable1, data$variable2 متغیرهای پیوسته شما
cor.test(data$variable1, data$variable2, method = "pearson")
        `,
        r_code_notes: "<p>ضریب همبستگی (r) بین -1 و +1 است. مقدار p-value معناداری همبستگی را نشان می‌دهد.</p><p>نمودار پراکندگی (<code>plot(data$variable1, data$variable2)</code>) برای مشاهده بصری رابطه مفید است.</p>"
    },
    test_spearman_correlation: {
        isFinal: true,
        name: "همبستگی اسپیرمن (Spearman Rank Correlation)",
        description: "برای سنجش قدرت و جهت رابطه یکنواخت (monotonic) بین دو متغیر پیوسته یا ترتیبی.",
        whenToUse: "زمانی که حداقل یکی از متغیرها ترتیبی است، یا زمانی که متغیرهای پیوسته پیش‌فرض نرمال بودن را برای همبستگی پیرسون برآورده نمی‌کنند، یا زمانی که انتظار رابطه یکنواخت (نه لزوماً خطی) دارید.",
        r_code: `
# data$variable1, data$variable2 متغیرهای شما (پیوسته یا ترتیبی)
cor.test(data$variable1, data$variable2, method = "spearman")
        `,
        r_code_notes: "<p>این آزمون بر اساس رتبه‌های داده‌ها کار می‌کند و به مقادیر پرت حساسیت کمتری دارد.</p>"
    },
    test_mann_whitney_u: { /* (Wilcoxon Rank Sum Test) ... کد از پیام قبلی ... */ },
    test_wilcoxon_signed_rank_paired: { /* ... کد از پیام قبلی ... */ },
    test_one_way_anova: { /* ... کد از پیام قبلی ... */ },
    test_kruskal_wallis: {
        isFinal: true,
        name: "آزمون کروسکال-والیس (Kruskal-Wallis Test)",
        description: "معادل ناپارامتریک تحلیل واریانس یک‌طرفه (One-Way ANOVA)، برای مقایسه میانگین رتبه‌های سه یا چند گروه مستقل.",
        whenToUse: "زمانی که یک متغیر وابسته پیوسته یا ترتیبی و یک متغیر مستقل طبقه‌ای با سه یا چند سطح دارید و پیش‌فرض‌های ANOVA (نرمال بودن، همگنی واریانس‌ها) برقرار نیست.",
        r_code: `
# data$dependent_var متغیر وابسته (پیوسته یا ترتیبی)
# data$group_var متغیر مستقل طبقه‌ای (فاکتور) با k سطح (k >= 3)
# data$group_var <- factor(data$group_var)
kruskal.test(dependent_var ~ group_var, data = data)

# برای آزمون‌های تعقیبی ناپارامتریک (مثلاً Dunn's test):
# install.packages("dunn.test")
# library(dunn.test)
# dunn.test(data$dependent_var, g = data$group_var, method = "bonferroni")
        `,
        r_code_notes: "<p>اگر نتیجه آزمون کروسکال-والیس معنادار باشد، از آزمون‌های تعقیبی ناپارامتریک برای مقایسه جفتی گروه‌ها استفاده کنید.</p>"
    },
    test_friedman_test: {
        isFinal: true,
        name: "آزمون فریدمن (Friedman Test)",
        description: "معادل ناپارامتریک تحلیل واریانس یک‌طرفه با اندازه‌گیری‌های مکرر، برای مقایسه میانگین رتبه‌ها در سه یا چند سطح از یک فاکتور درون‌آزمودنی.",
        whenToUse: "زمانی که یک متغیر وابسته پیوسته یا ترتیبی دارید که سه بار یا بیشتر روی یک گروه از افراد اندازه‌گیری شده و پیش‌فرض کرویت برای ANOVA مکرر برقرار نیست یا داده‌ها به شدت غیرنرمال هستند.",
        r_code: `
# داده‌ها باید در فرمت ماتریسی باشند (ستون‌ها = شرایط/زمان‌ها، ردیف‌ها = آزمودنی‌ها)
# یا در فرمت long با تابع friedman.test(y ~ groups | blocks, data = mydata_long)

# مثال با داده‌های ماتریسی (فرض کنید Y1, Y2, Y3 سه اندازه‌گیری هستند)
# my_matrix_data <- cbind(data$Y1, data$Y2, data$Y3)
# friedman.test(my_matrix_data)

# مثال با فرمت long:
# data_long شامل ستون‌های: id (شناسه آزمودنی), time (فاکتور درون‌آزمودنی), value (متغیر وابسته)
# friedman.test(value ~ time | id, data = data_long)
        `,
        r_code_notes: "<p>اگر داده‌ها در فرمت long هستند، فرمول <code>y ~ groups | blocks</code> استفاده می‌شود که <code>y</code> متغیر وابسته، <code>groups</code> فاکتور درون‌آزمودنی، و <code>blocks</code> شناسه آزمودنی‌هاست.</p><p>برای آزمون‌های تعقیبی ناپارامتریک (مثلاً آزمون Nemenyi یا Wilcoxon جفتی با تصحیح)، پکیج‌های <code>PMCMRplus</code> یا <code>scmamp</code> را بررسی کنید.</p>"
    },
    test_one_way_repeated_measures_anova: { /* ... کد از پیام قبلی ... */ },
    test_multiple_regression: { /* ... کد از پیام قبلی ... */ },
    test_ancova_or_regression_with_dummies: { /* ... کد از پیام قبلی ... */ },
    test_factorial_anova_independent: { /* ... کد از پیام قبلی ... */ },
    test_factorial_anova_repeated_measures: { /* ... کد از پیام قبلی ... */ },
    test_mixed_anova: { /* ... کد از پیام قبلی ... */ },
    test_anova_main_effects_only_independent: { /* ... کد از پیام قبلی ... */ },
    test_repeated_measures_anova_main_effects_only: { /* ... کد از پیام قبلی ... */ },
    test_manova: { /* ... کد از پیام قبلی ... */ },
    test_factorial_manova: { /* ... کد از پیام قبلی ... */ },
    test_multivariate_multiple_regression: { /* ... کد از پیام قبلی ... */ },
    test_canonical_correlation: {
        isFinal: true,
        name: "همبستگی کانونی (Canonical Correlation Analysis - CCA)",
        description: "برای بررسی رابطه بین دو مجموعه از متغیرهای پیوسته (یک مجموعه متغیرهای X و یک مجموعه متغیرهای Y).",
        whenToUse: "زمانی که می‌خواهید همبستگی بین ترکیب‌های خطی از دو مجموعه متغیر را پیدا کنید. این روش چندین جفت از ترکیب‌های خطی (متغیرهای کانونی) را که بیشترین همبستگی را با هم دارند، استخراج می‌کند.",
        r_code: `
# نیاز به پکیج CCA یا candisc
# install.packages("CCA")
library(CCA)

# X_vars یک ماتریس از مجموعه اول متغیرها
# Y_vars یک ماتریس از مجموعه دوم متغیرها
# X_vars <- as.matrix(data[, c("X1", "X2")])
# Y_vars <- as.matrix(data[, c("Y1", "Y2", "Y3")])

# cc_result <- cc(X_vars, Y_vars)
# print(cc_result$cor) # همبستگی‌های کانونی

# برای آزمون معناداری همبستگی‌ها:
# install.packages("candisc")
# library(candisc)
# model_cca <- lm(cbind(Y1, Y2, Y3) ~ X1 + X2, data=data)
# can_corr_test <- manova(model_cca) # از طریق MANOVA برای آزمون Wilks' Lambda
# summary(can_corr_test, test="Wilks") 
# یا استفاده از پکیج‌های تخصصی‌تر برای آزمون‌های CCA
        `,
        r_code_notes: "<p>پکیج <code>CCA</code> یا <code>candisc</code> برای این تحلیل مفید هستند.</p><p>تفسیر نتایج CCA می‌تواند پیچیده باشد و شامل بررسی همبستگی‌های کانونی، بارهای کانونی و واریانس مشترک است.</p>"
    },
    info_set_of_simple_regressions: {
        isFinal: true,
        name: "اطلاعات: اجرای مجموعه‌ای از رگرسیون‌های ساده/چندگانه",
        description: "زمانی که چندین متغیر وابسته پیوسته دارید و می‌خواهید هر یک را به طور جداگانه توسط یک یا چند متغیر مستقل مدل‌سازی کنید، می‌توانید برای هر متغیر وابسته یک مدل رگرسیون جداگانه اجرا کنید.",
        whenToUse: "این روش زمانی مناسب است که متغیرهای وابسته از نظر مفهومی متمایز هستند و علاقه‌ای به بررسی روابط همزمان بین آن‌ها (مانند MANOVA یا رگرسیون چندمتغیره) ندارید. در صورت اجرای تعداد زیادی آزمون، به مشکل آزمون‌های چندگانه (multiple testing problem) و نیاز به تصحیح سطح آلفا (مانند تصحیح بونفرونی) توجه کنید.",
        r_code: `
# فرض کنید Y1, Y2, Y3 متغیرهای وابسته و X1, X2 متغیرهای مستقل هستند
# model_Y1 <- lm(Y1 ~ X1 + X2, data = data)
# summary(model_Y1)

# model_Y2 <- lm(Y2 ~ X1 + X2, data = data)
# summary(model_Y2)

# model_Y3 <- lm(Y3 ~ X1 + X2, data = data)
# summary(model_Y3)
        `,
        r_code_notes: "<p>هر مدل به طور مستقل تفسیر می‌شود. اگر تعداد زیادی مدل اجرا می‌کنید، مراقب افزایش احتمال خطای نوع اول باشید و از روش‌های تصحیح برای آزمون‌های چندگانه استفاده کنید.</p>"
    },

    // ## آزمون‌های مربوط به یک متغیر وابسته طبقه‌ای ##
    test_binomial_test_or_chi_sq_gof_binary: {
        question: "آیا فراوانی مورد انتظار برای هر دو سطح متغیر دوتایی شما حداقل 5 است؟",
        options: [
            { text: "بله", nextNode: "test_chi_square_goodness_of_fit_binary" }, // Chi-sq GOF for binary
            { text: "خیر، یا می‌خواهم از آزمون دقیق استفاده کنم.", nextNode: "test_binomial_test" }
        ]
    },
    test_binomial_test: {
        isFinal: true,
        name: "آزمون دوجمله‌ای (Binomial Test)",
        description: "برای مقایسه نسبت مشاهده شده یک متغیر دوتایی با یک نسبت مورد انتظار (نظری).",
        whenToUse: "زمانی که یک متغیر دوتایی (دو پیامد ممکن) دارید و می‌خواهید بدانید آیا نسبت یکی از پیامدها در نمونه شما به طور معناداری با یک مقدار p0 (احتمال فرضی) تفاوت دارد. این آزمون دقیق است و برای حجم نمونه کوچک نیز مناسب است.",
        r_code: `
# x: تعداد موفقیت‌های مشاهده شده
# n: تعداد کل آزمایش‌ها (حجم نمونه)
# p0: احتمال موفقیت مورد انتظار تحت فرضیه صفر
# binom.test(x = 50, n = 100, p = 0.55) # مثال: 50 موفقیت در 100 آزمایش، با p0=0.55
        `,
        r_code_notes: "<p>اگر داده‌های شما به صورت تعداد موفقیت‌ها و تعداد کل آزمایش‌ها نیست، ابتدا باید آن‌ها را محاسبه کنید.</p>"
    },
    test_chi_square_goodness_of_fit_binary: {
        isFinal: true,
        name: "آزمون خی‌دو نیکویی برازش (برای متغیر دوتایی)",
        description: "برای مقایسه فراوانی‌های مشاهده شده یک متغیر دوتایی با فراوانی‌های مورد انتظار.",
        whenToUse: "زمانی که یک متغیر دوتایی دارید و می‌خواهید بدانید آیا توزیع فراوانی آن با یک توزیع مشخص (تعیین شده توسط احتمالات مورد انتظار برای هر دو سطح) مطابقت دارد. فراوانی‌های مورد انتظار باید به اندازه کافی بزرگ باشند (معمولاً ≥ 5).",
        r_code: `
# observed_freqs <- c(60, 40) # مثال: 60 مشاهده در سطح 1، 40 در سطح 0
# expected_probs <- c(0.5, 0.5) # مثال: انتظار توزیع یکنواخت
# chisq.test(x = observed_freqs, p = expected_probs)
        `,
        r_code_notes: "<p>این همان آزمون خی‌دو نیکویی برازش عمومی است که برای حالت خاص دو سطح استفاده می‌شود.</p>"
    },
    info_descriptive_frequencies_binary: { /* مشابه info_descriptive_frequencies اما برای دو سطح */ },
    test_chi_square_test_of_independence_or_fisher: {
        question: "آیا در جدول توافقی 2x2 شما، تمام فراوانی‌های مورد انتظار حداقل 5 هستند؟",
        options: [
            { text: "بله", nextNode: "test_chi_square_test_of_independence" },
            { text: "خیر (استفاده از آزمون دقیق فیشر).", nextNode: "test_fisher_exact_test" }
        ]
    },
    test_chi_square_test_of_independence: { /* ... کد از پیام قبلی ... */ },
    test_fisher_exact_test: {
        isFinal: true,
        name: "آزمون دقیق فیشر (Fisher's Exact Test)",
        description: "برای بررسی استقلال دو متغیر طبقه‌ای در جداول توافقی کوچک (معمولاً 2x2) که پیش‌فرض‌های آزمون خی‌دو (فراوانی‌های مورد انتظار بزرگ) برقرار نیست.",
        whenToUse: "زمانی که حداقل یک سلول در جدول توافقی 2x2 شما فراوانی مورد انتظار کمتر از 5 دارد. این آزمون مقدار p دقیق را محاسبه می‌کند.",
        r_code: `
# contingency_table یک ماتریس 2x2 از فراوانی‌های مشاهده شده
# مثال:
# my_table <- matrix(c(3, 1, 2, 8), nrow = 2, byrow = TRUE)
# fisher.test(my_table)
        `,
        r_code_notes: "<p>این آزمون برای جداول بزرگتر از 2x2 نیز قابل استفاده است اما محاسبات آن ممکن است سنگین شود (R می‌تواند آن را با شبیه‌سازی انجام دهد).</p>"
    },
    test_mcnemar_test: { /* ... کد از پیام قبلی ... */ },
    test_logistic_regression_simple: { /* ... کد از پیام قبلی ... */ },
    test_multiple_logistic_regression: { /* ... کد از پیام قبلی ... */ },
    test_multinomial_logistic_regression: { /* ... کد از پیام قبلی ... */ },
    test_ordinal_logistic_regression: { /* ... کد از پیام قبلی ... */ },
    info_descriptive_ordinal_or_gof: {
        isFinal: true,
        name: "آمار توصیفی برای داده‌های ترتیبی یا نیکویی برازش",
        description: "توصیف توزیع یک متغیر ترتیبی (مانند جداول فراوانی، میانه، مد) یا بررسی اینکه آیا توزیع آن با یک توزیع نظری مطابقت دارد.",
        whenToUse: "زمانی که می‌خواهید ویژگی‌های اصلی یک متغیر ترتیبی را خلاصه کنید یا آن را با یک توزیع خاص مقایسه کنید.",
        r_code: `
# data$ordinal_var متغیر ترتیبی شما (باید factor مرتب شده باشد)
# data$ordinal_var <- factor(data$ordinal_var, ordered = TRUE, levels = c("کم", "متوسط", "زیاد"))

# آمار توصیفی
# print(summary(data$ordinal_var)) # جدول فراوانی
# print(median(as.numeric(data$ordinal_var))) # میانه (نیاز به تبدیل به عددی با احتیاط)

# برای نیکویی برازش، مشابه آزمون خی‌دو برای متغیرهای طبقه‌ای عمل می‌شود
# observed_frequencies <- table(data$ordinal_var)
# expected_probabilities <- c(0.2, 0.5, 0.3) # مثال
# chisq.test(x = observed_frequencies, p = expected_probabilities)
        `,
        r_code_notes: "<p>برای آمار توصیفی، جداول فراوانی و درصدها، میانه و مد مناسب هستند. میانگین برای داده‌های ترتیبی معمولاً توصیه نمی‌شود مگر اینکه فواصل بین سطوح برابر فرض شود.</p><p>برای نیکویی برازش، می‌توان از آزمون خی‌دو استفاده کرد، اما به یاد داشته باشید که ترتیب سطوح را در نظر نمی‌گیرد.</p>"
    },
    test_mann_whitney_or_kruskal_wallis_for_ordinal_dv: {
        question: "متغیر مستقل طبقه‌ای شما چند سطح دارد؟",
        options: [
            { text: "دو سطح (مقایسه دو گروه مستقل)", nextNode: "test_mann_whitney_u" },
            { text: "سه یا چند سطح (مقایسه چند گروه مستقل)", nextNode: "test_kruskal_wallis" }
        ]
    },
    test_log_linear_model_or_mca: { /* ... کد از پیام قبلی ... */ },

    // ## آزمون‌های مربوط به یک متغیر وابسته شمارشی (Count) ##
    test_poisson_goodness_of_fit: { /* ... کد از پیام قبلی ... */ },
    info_descriptive_count: { /* ... کد از پیام قبلی ... */ },
    test_poisson_regression: { /* ... کد از پیام قبلی ... */ },
    test_poisson_regression_offset: { /* ... کد از پیام قبلی ... */ },
    test_negative_binomial_regression: { /* ... کد از پیام قبلی ... */ },
    test_negative_binomial_regression_offset: { /* ... کد از پیام قبلی ... */ },
    test_zero_inflated_poisson_or_nb: { /* ... کد از پیام قبلی ... */ },
    test_zero_inflated_poisson_or_nb_offset: {
        isFinal: true,
        name: "مدل‌های شمارشی با تورم صفر و متغیر جبرانی (Offset)",
        description: "برای مدل‌سازی داده‌های شمارشی با تورم صفر که نیاز به لحاظ کردن متغیر جبرانی (offset) برای استانداردسازی نرخ‌ها دارند.",
        whenToUse: "زمانی که هم تورم صفر دارید و هم دوره مشاهده یا میزان 'قرار گرفتن در معرض' برای هر واحد متفاوت است.",
        r_code: `
# نیاز به پکیج pscl یا glmmTMB
library(pscl)

# data$count_var متغیر وابسته شمارشی
# data$exposure_var متغیر جبرانی
# data$ind_var_count, data$ind_var_zero متغیرهای مستقل برای بخش شمارش و تورم صفر

# مدل پواسون با تورم صفر و offset (ZIP Offset)
# model_zip_offset <- zeroinfl(count_var ~ ind_var_count + offset(log(exposure_var)) | ind_var_zero,
#                              data = data,
#                              dist = "poisson")
# summary(model_zip_offset)

# مدل دوجمله‌ای منفی با تورم صفر و offset (ZINB Offset)
# model_zinb_offset <- zeroinfl(count_var ~ ind_var_count + offset(log(exposure_var)) | ind_var_zero,
#                               data = data,
#                               dist = "negbin")
# summary(model_zinb_offset)
        `,
        r_code_notes: "<p>متغیر <code>offset(log(exposure_var))</code> به بخش شمارشی فرمول (قبل از <code>|</code>) اضافه می‌شود.</p><p>انتخاب بین ZIP و ZINB با offset بستگی به وجود پراکندگی بیش از حد (علاوه بر تورم صفر) در بخش شمارشی دارد.</p>"
    }

    // ... (اطمینان حاصل کنید که تمام گره‌های test_... و info_... که در مسیرها به آن‌ها ارجاع داده شده، تعریف شده‌اند) ...
    // ... (گره‌هایی مانند test_chi_square_goodness_of_fit و بقیه که در پیام قبلی کدشان بود، اینجا باید باشند) ...
    // ... (من فقط موارد جدیدتر یا اصلاح شده را در اینجا اضافه کردم. شما باید کدهای قبلی را هم داشته باشید) ...
};


// بقیه کد JavaScript (توابع displayNode, resetGuide, copyCodeToClipboard و Event Listeners)
// دقیقاً همان کدی است که در پیام قبلی (پیام با عنوان "بسیار خب، عالیه! بریم که این پروژه رو حسابی پولیش کنیم...") برایتان ارسال کردم.
// لطفاً آن بخش را از آن پیام کپی کرده و در اینجا، بعد از پایان آبجکت decisionTree قرار دهید.
// از خط let currentNodeKey = 'start'; به بعد.

let currentNodeKey = 'start';
let userPath = [];

function displayNode(nodeKey) {
    const node = decisionTree[nodeKey];
    if (!node) {
        console.error("خطای داخلی: گره با کلید", nodeKey, "یافت نشد. لطفاً ساختار decisionTree را بررسی کنید.");
        questionTextElement.textContent = "خطا: یک مشکل داخلی رخ داده است. لطفاً به توسعه‌دهنده اطلاع دهید.";
        optionsContainerElement.innerHTML = "";
        resultContainerElement.classList.add('hidden');
        userPathContainerElement.classList.add('hidden');
        return;
    }
    currentNodeKey = nodeKey;

    if (node.isFinal) {
        questionContainerElement.classList.add('hidden');
        resultContainerElement.classList.remove('hidden');
        resetButtonElement.classList.remove('hidden');
        testNameElement.textContent = node.name || "نام آزمون مشخص نشده";
        testDescriptionElement.innerHTML = node.description ? node.description.replace(/\n/g, '<br>') : "توضیحات برای این آزمون ارائه نشده است.";
        testWhenToUseElement.innerHTML = node.whenToUse ? node.whenToUse.replace(/\n/g, '<br>') : "اطلاعاتی در مورد زمان استفاده از این آزمون موجود نیست.";
        if (node.r_code && node.r_code.trim() !== "") {
            rCodeElement.textContent = node.r_code.trim();
            if (typeof Prism !== 'undefined' && Prism.highlightElement) {
                Prism.highlightElement(rCodeElement);
            }
            copyCodeButtonElement.classList.remove('hidden');
        } else {
            rCodeElement.textContent = "کد R نمونه برای این آزمون ارائه نشده است.";
            copyCodeButtonElement.classList.add('hidden');
        }
        rCodeNotesElement.innerHTML = node.r_code_notes || "";
        if (userPath.length > 0) {
            userPathListElement.innerHTML = '';
            userPath.forEach(stepText => {
                const listItem = document.createElement('li');
                listItem.textContent = stepText;
                userPathListElement.appendChild(listItem);
            });
            userPathContainerElement.classList.remove('hidden');
        } else {
            userPathContainerElement.classList.add('hidden');
        }
    } else {
        resultContainerElement.classList.add('hidden');
        userPathContainerElement.classList.add('hidden');
        questionContainerElement.classList.remove('hidden');
        if (nodeKey !== 'start') {
             resetButtonElement.classList.remove('hidden');
        } else {
            resetButtonElement.classList.add('hidden');
        }
        questionTextElement.textContent = node.question;
        optionsContainerElement.innerHTML = "";
        if (node.options && node.options.length > 0) {
            node.options.forEach(option => {
                const button = document.createElement('button');
                button.textContent = option.text;
                button.onclick = () => {
                    userPath.push(`"${node.question}" ← ${option.text}`);
                    displayNode(option.nextNode);
                };
                optionsContainerElement.appendChild(button);
            });
        } else {
            console.warn("گره سوال", nodeKey, "هیچ گزینه‌ای (options) ندارد.");
            optionsContainerElement.innerHTML = "<p>گزینه‌ای برای این مرحله تعریف نشده است.</p>";
        }
    }
}

function resetGuide() {
    userPath = [];
    if (copyButtonTextElement && copyCodeButtonElement) {
        copyButtonTextElement.textContent = "کپی";
        copyCodeButtonElement.classList.remove('copied');
        copyCodeButtonElement.disabled = false;
    }
    displayNode('start');
}

function copyCodeToClipboard() {
    const codeToCopy = rCodeElement.innerText;
    if (!codeToCopy || codeToCopy.trim() === "" || codeToCopy.includes("ارائه نشده است")) {
        console.warn("محتوایی برای کپی وجود ندارد.");
        return;
    }
    navigator.clipboard.writeText(codeToCopy).then(() => {
        copyButtonTextElement.textContent = 'کپی شد!';
        copyCodeButtonElement.classList.add('copied');
        copyCodeButtonElement.disabled = true;
        setTimeout(() => {
            copyButtonTextElement.textContent = 'کپی';
            copyCodeButtonElement.classList.remove('copied');
            copyCodeButtonElement.disabled = false;
        }, 2500);
    }).catch(err => {
        console.warn('خطا در کپی با Clipboard API:', err);
        try {
            const textArea = document.createElement('textarea');
            textArea.value = codeToCopy;
            textArea.style.position = 'fixed';
            textArea.style.top = '-9999px';
            textArea.style.left = '-9999px';
            document.body.appendChild(textArea);
            textArea.focus();
            textArea.select();
            const successful = document.execCommand('copy');
            document.body.removeChild(textArea);
            if (successful) {
                copyButtonTextElement.textContent = 'کپی شد!';
                copyCodeButtonElement.classList.add('copied');
                copyCodeButtonElement.disabled = true;
                setTimeout(() => {
                    copyButtonTextElement.textContent = 'کپی';
                    copyCodeButtonElement.classList.remove('copied');
                    copyCodeButtonElement.disabled = false;
                }, 2500);
            } else {
                throw new Error('کپی با execCommand ناموفق بود.');
            }
        } catch (e) {
            console.error('خطا در کپی با execCommand (fallback):', e);
            copyButtonTextElement.textContent = 'خطا!';
        }
    });
}

if (resetButtonElement) {
    resetButtonElement.addEventListener('click', resetGuide);
} else {
    console.error("دکمه ریست یافت نشد.");
}
if (copyCodeButtonElement) {
    copyCodeButtonElement.addEventListener('click', copyCodeToClipboard);
} else {
    console.error("دکمه کپی کد یافت نشد.");
}

document.addEventListener('DOMContentLoaded', () => {
    displayNode('start');
});
