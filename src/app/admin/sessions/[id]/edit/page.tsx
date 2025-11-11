'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { userSessionService } from '@/services/userSessionService';
import { UpdateUserSessionRequest } from '@/types/user-session';
import { Input, Button } from '@/components/ui';

export default function EditUserSessionPage() {
  const params = useParams();
  const router = useRouter();
  const [formData, setFormData] = useState<UpdateUserSessionRequest>({
    programName: '',
    reenterFlag: false,
  });
  const [loading, setLoading] = useState(true);
  const [saving, setSaving] = useState(false);
  const [errors, setErrors] = useState<Record<string, string>>({});

  useEffect(() => {
    if (params.id) {
      fetchSession(Number(params.id));
    }
  }, [params.id]);

  const fetchSession = async (id: number) => {
    try {
      const data = await userSessionService.getUserSessionById(id);
      setFormData({
        programName: data.programName,
        reenterFlag: data.reenterFlag,
      });
    } catch (err) {
      console.error('Failed to load session:', err);
    } finally {
      setLoading(false);
    }
  };

  const validateForm = (): boolean => {
    const newErrors: Record<string, string> = {};

    if (!formData.programName?.trim()) {
      newErrors.programName = 'Program name is required';
    } else if (formData.programName.length > 8) {
      newErrors.programName = 'Program name must be 8 characters or less';
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
      await userSessionService.updateUserSession(Number(params.id), formData);
      router.push(`/admin/sessions/${params.id}`);
    } catch (err) {
      alert('Failed to update session');
      console.error(err);
    } finally {
      setSaving(false);
    }
  };

  if (loading) return <div className="p-6">Loading...</div>;

  return (
    <div className="p-6 max-w-2xl">
      <h1 className="text-2xl font-bold mb-6">Edit User Session</h1>
      
      <form onSubmit={handleSubmit} className="space-y-4">
        <div>
          <Input
            label="Program Name"
            value={formData.programName || ''}
            onChange={(e) => {
              setFormData({ ...formData, programName: e.target.value.toUpperCase() });
              setErrors({ ...errors, programName: '' });
            }}
            required
            maxLength={8}
          />
          {errors.programName && (
            <p className="mt-1 text-sm text-red-600">{errors.programName}</p>
          )}
          <p className="mt-1 text-sm text-gray-500">Maximum 8 characters</p>
        </div>
        
        <div className="flex items-center">
          <input
            type="checkbox"
            id="reenterFlag"
            checked={formData.reenterFlag}
            onChange={(e) => setFormData({ ...formData, reenterFlag: e.target.checked })}
            className="h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
          />
          <label htmlFor="reenterFlag" className="ml-2 block text-sm text-gray-900">
            Reenter Flag
          </label>
        </div>
        
        <div className="flex gap-2 pt-4">
          <Button type="submit" disabled={saving}>
            {saving ? 'Saving...' : 'Save Changes'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push(`/admin/sessions/${params.id}`)}
          >
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
