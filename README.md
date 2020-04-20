Image and shape processing tools implemented in Ada.

What's implemented:

### Image processing
- Basic Morphology erode / dilate (GPU [opencl], CPU)
- Bernsen Adaptative Thresholding (GPU [opencl], CPU)
- Connected Component Labeling and region detection (GPU [opencl], CPU)
- Image Moments (Hu) (CPU)
- Filtering (Gaussian) (GPU [opencl], CPU)
- Histogram generation and basic operations (Jensen-Shannon / Kullback-Leibler divergence) (CPU)

### Neural Network framework
- Training (CPU)
- Inference (GPU[opencl], CPU)
- Persistence
- TODO: add more optimizers
