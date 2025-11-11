'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { adminMenuOptionService } from '@/services/adminMenuOptionService';
import { UpdateAdminMenuOptionRequest } from '@/types/admin-menu-option';
import { Input, Button } from '@/components/ui';

export default function EditAdminMenuOptionPage() {
  const params = useParams();
  const router = useRouter();
  const [formData, setFormData] = useState<UpdateAdminMenuOptionRequest>({
    optionName: '',
    isActive: true,
  });
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [errors, setErrors] = useState<Record<string, string>>({});

  useEffect(() => {
    if (params.id) {
      fetchMenuOption(Number(params.id));
    }
  }, [params.id]);

  const fetchMenuOption = async (id: number) => {
    try {
      const data = await adminMenuOptionService.getAdminMenuOptionById(id);
      setFormData({
        optionName: data.optionName,
        isActive: data.isActive,
      });
    } catch (err) {
      console.error('Failed to load menu option:', err);
    } finally {
      setLoading(false);
    }
  };

  const validateForm = (): boolean => {
    const newErrors: Record<string, string> = {};

    if (!formData.optionName?.trim()) {
      newErrors.optionName = 'Option name is required';
    } else if (formData.optionName.length > 35) {
      newErrors.optionName = 'Option name must be 35 characters or less';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm()) {
      return;
    }

    try {
      setSaving(true);
      await adminMenuOptionService.updateAdminMenuOption(Number(params.id), formData);
      router.push(`/admin/menu-options/${params.id}`);
    } catch (err) {
      alert('Failed to update menu option');
      console.error(err);
    } finally {
      setSaving(false);
    }
  };

  if (loading) return <div className="p-6">Loading...</div>;

  return (
    <div className="p-6 max-w-2xl">
      <h1 className="text-2xl font-bold mb-6">Edit Menu Option</h1>
      
      <form onSubmit={handleSubmit} className="space-y-4">
        <div>
          <Input
            label="Option Name"
            value={formData.optionName || ''}
            onChange={(e) => {
              setFormData({ ...formData, optionName: e.target.value });
              setErrors({ ...errors, optionName: '' });
            }}
            required
            maxLength={35}
          />
          {errors.optionName && (
            <p className="mt-1 text-sm text-red-600">{errors.optionName}</p>
          )}
          <p className="mt-1 text-sm text-gray-500">Maximum 35 characters</p>
        </div>
        
        <div className="flex items-center">
          <input
            type="checkbox"
            id="isActive"
            checked={formData.isActive}
            onChange={(e) => setFormData({ ...formData, isActive: e.target.checked })}
            className="h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
          />
          <label htmlFor="isActive" className="ml-2 block text-sm text-gray-900">
            Active (uncheck for "Coming Soon" status)
          </label>
        </div>
        
        <div className="flex gap-2 pt-4">
          <Button type="submit" disabled={saving}>
            {saving ? 'Saving...' : 'Save Changes'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push(`/admin/menu-options/${params.id}`)}
          >
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
