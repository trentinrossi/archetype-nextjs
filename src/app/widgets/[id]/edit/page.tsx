'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { Button, Input } from '@/components/ui';
import { widgetService } from '@/services/widgetService';
import { UpdateWidgetRequest } from '@/types/widget';

export default function EditWidgetPage() {
  const params = useParams();
  const router = useRouter();
  const widgetId = params.id as string;
  
  const [loading, setLoading] = useState(true);
  const [submitting, setSubmitting] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const [formData, setFormData] = useState<UpdateWidgetRequest>({
    name: '',
    category: '',
    price: 0,
    isActive: true,
    description: '',
  });

  const [fieldErrors, setFieldErrors] = useState<Record<string, string>>({});

  useEffect(() => {
    const fetchWidget = async () => {
      try {
        setLoading(true);
        setError(null);
        const data = await widgetService.getWidgetById(widgetId);
        
        // Populate form with existing data
        setFormData({
          name: data.name,
          category: data.category,
          price: data.price,
          isActive: data.isActive,
          description: data.description || '',
        });
      } catch (err) {
        console.error('Error fetching widget:', err);
        setError(err instanceof Error ? err.message : 'Failed to fetch widget');
      } finally {
        setLoading(false);
      }
    };

    if (widgetId) {
      fetchWidget();
    }
  }, [widgetId]);

  const handleInputChange = (e: React.ChangeEvent<HTMLInputElement | HTMLSelectElement | HTMLTextAreaElement>) => {
    const { name, value, type } = e.target;
    
    // Clear field error when user starts typing
    if (fieldErrors[name]) {
      setFieldErrors(prev => {
        const newErrors = { ...prev };
        delete newErrors[name];
        return newErrors;
      });
    }

    // Handle different input types
    if (type === 'number') {
      setFormData(prev => ({
        ...prev,
        [name]: value === '' ? 0 : parseFloat(value),
      }));
    } else if (type === 'checkbox') {
      setFormData(prev => ({
        ...prev,
        [name]: (e.target as HTMLInputElement).checked,
      }));
    } else {
      setFormData(prev => ({
        ...prev,
        [name]: value,
      }));
    }
  };

  const validateForm = (): boolean => {
    const errors: Record<string, string> = {};

    if (formData.name && !formData.name.trim()) {
      errors.name = 'Name cannot be empty';
    }

    if (formData.category && !formData.category.trim()) {
      errors.category = 'Category cannot be empty';
    }

    if (formData.price !== undefined && formData.price < 0) {
      errors.price = 'Price cannot be negative';
    }

    setFieldErrors(errors);
    return Object.keys(errors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm()) {
      setError('Please fix the errors in the form');
      return;
    }

    try {
      setSubmitting(true);
      setError(null);

      await widgetService.updateWidget(widgetId, formData);
      
      // Redirect to widget details on success
      router.push(`/widgets/${widgetId}`);
    } catch (err) {
      console.error('Error updating widget:', err);
      setError(err instanceof Error ? err.message : 'Failed to update widget');
      setSubmitting(false);
    }
  };

  const handleCancel = () => {
    router.push(`/widgets/${widgetId}`);
  };

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-gray-900"></div>
          <p className="mt-4 text-gray-600">Loading widget...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="container mx-auto px-4 py-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Edit Widget</h1>
        <p className="text-gray-600">Update widget information for widget {widgetId}</p>
      </div>

      <div className="bg-white shadow-md rounded-lg overflow-hidden max-w-2xl">
        <form onSubmit={handleSubmit} className="p-6">
          {error && (
            <div className="mb-6 bg-red-50 border border-red-200 text-red-800 px-4 py-3 rounded-md">
              <div className="flex">
                <div className="flex-shrink-0">
                  <svg className="h-5 w-5 text-red-400" viewBox="0 0 20 20" fill="currentColor">
                    <path fillRule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z" clipRule="evenodd" />
                  </svg>
                </div>
                <div className="ml-3">
                  <p className="text-sm font-medium">{error}</p>
                </div>
              </div>
            </div>
          )}

          <div className="space-y-6">
            {/* Name */}
            <div>
              <label htmlFor="name" className="block text-sm font-medium text-gray-700 mb-1">
                Name <span className="text-red-500">*</span>
              </label>
              <Input
                id="name"
                name="name"
                type="text"
                value={formData.name || ''}
                onChange={handleInputChange}
                placeholder="Enter widget name"
                className={fieldErrors.name ? 'border-red-500' : ''}
                disabled={submitting}
                required
              />
              {fieldErrors.name && (
                <p className="mt-1 text-sm text-red-600">{fieldErrors.name}</p>
              )}
            </div>

            {/* Category */}
            <div>
              <label htmlFor="category" className="block text-sm font-medium text-gray-700 mb-1">
                Category <span className="text-red-500">*</span>
              </label>
              <Input
                id="category"
                name="category"
                type="text"
                value={formData.category || ''}
                onChange={handleInputChange}
                placeholder="Enter category"
                className={fieldErrors.category ? 'border-red-500' : ''}
                disabled={submitting}
                required
              />
              {fieldErrors.category && (
                <p className="mt-1 text-sm text-red-600">{fieldErrors.category}</p>
              )}
            </div>

            {/* Price */}
            <div>
              <label htmlFor="price" className="block text-sm font-medium text-gray-700 mb-1">
                Price <span className="text-red-500">*</span>
              </label>
              <Input
                id="price"
                name="price"
                type="number"
                step="0.01"
                value={formData.price || 0}
                onChange={handleInputChange}
                placeholder="0.00"
                className={fieldErrors.price ? 'border-red-500' : ''}
                disabled={submitting}
                required
              />
              {fieldErrors.price && (
                <p className="mt-1 text-sm text-red-600">{fieldErrors.price}</p>
              )}
            </div>

            {/* Is Active */}
            <div className="flex items-center">
              <input
                id="isActive"
                name="isActive"
                type="checkbox"
                checked={formData.isActive}
                onChange={handleInputChange}
                disabled={submitting}
                className="h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
              />
              <label htmlFor="isActive" className="ml-2 block text-sm text-gray-700">
                Active
              </label>
            </div>

            {/* Description */}
            <div>
              <label htmlFor="description" className="block text-sm font-medium text-gray-700 mb-1">
                Description
              </label>
              <textarea
                id="description"
                name="description"
                value={formData.description || ''}
                onChange={handleInputChange}
                placeholder="Enter description (optional)"
                rows={4}
                disabled={submitting}
                className="flex w-full rounded-md border border-gray-300 bg-white px-3 py-2 text-sm focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent disabled:cursor-not-allowed disabled:opacity-50"
              />
            </div>
          </div>

          {/* Information Box */}
          <div className="mt-6 bg-blue-50 border border-blue-200 rounded-md p-4">
            <div className="flex">
              <div className="flex-shrink-0">
                <svg className="h-5 w-5 text-blue-400" viewBox="0 0 20 20" fill="currentColor">
                  <path fillRule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clipRule="evenodd" />
                </svg>
              </div>
              <div className="ml-3">
                <h3 className="text-sm font-medium text-blue-800">Update Information</h3>
                <div className="mt-2 text-sm text-blue-700">
                  <p>The widget ID and creation date cannot be changed.</p>
                </div>
              </div>
            </div>
          </div>

          {/* Form Actions */}
          <div className="mt-8 flex justify-end gap-3 pt-6 border-t border-gray-200">
            <Button
              type="button"
              variant="secondary"
              onClick={handleCancel}
              disabled={submitting}
            >
              Cancel
            </Button>
            <Button
              type="submit"
              disabled={submitting}
            >
              {submitting ? 'Saving...' : 'Save Changes'}
            </Button>
          </div>
        </form>
      </div>
    </div>
  );
}
