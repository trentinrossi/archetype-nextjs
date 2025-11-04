'use client';

import React, { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { useAuth } from '@/contexts/AuthContext';
import { Input, Button } from '@/components/ui';

export default function LoginPage() {
  const router = useRouter();
  const { login, isAuthenticated, user } = useAuth();
  const [formData, setFormData] = useState({
    userId: '',
    password: '',
  });
  const [errors, setErrors] = useState<{ userId?: string; password?: string; general?: string }>({});
  const [loading, setLoading] = useState(false);
  const [currentDateTime, setCurrentDateTime] = useState('');

  useEffect(() => {
    const updateDateTime = () => {
      const now = new Date();
      const date = now.toLocaleDateString('en-US', { month: '2-digit', day: '2-digit', year: '2-digit' });
      const time = now.toLocaleTimeString('en-US', { hour: '2-digit', minute: '2-digit', second: '2-digit', hour12: false });
      setCurrentDateTime(`${date} ${time}`);
    };

    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    return () => clearInterval(interval);
  }, []);

  useEffect(() => {
    if (isAuthenticated && user) {
      if (user.userType === 'A') {
        router.push('/admin');
      } else {
        router.push('/menu');
      }
    }
  }, [isAuthenticated, user, router]);

  const validateForm = (): boolean => {
    const newErrors: { userId?: string; password?: string } = {};

    if (!formData.userId.trim()) {
      newErrors.userId = 'Please enter User ID ...';
    }

    if (!formData.password.trim()) {
      newErrors.password = 'Please enter Password ...';
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
      setLoading(true);
      setErrors({});

      const success = await login(formData.userId, formData.password);
      
      if (!success) {
        setErrors({ general: 'Wrong Password. Try again ...' });
      }
    } catch (error) {
      console.error('Login error:', error);
      setErrors({ general: 'Unable to verify the User ...' });
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="min-h-screen flex items-center justify-center bg-gradient-to-br from-blue-50 to-indigo-100 p-4">
      <div className="w-full max-w-md">
        <div className="bg-white rounded-2xl shadow-2xl overflow-hidden">
          {/* Header */}
          <div className="bg-gradient-to-r from-blue-600 to-indigo-600 px-8 py-6">
            <div className="flex justify-between items-center mb-2">
              <h1 className="text-2xl font-bold text-white">CC00</h1>
              <p className="text-blue-100 text-sm">{currentDateTime}</p>
            </div>
            <h2 className="text-xl font-semibold text-white text-center">CardDemo Application</h2>
            <p className="text-blue-100 text-center text-sm mt-1">Sign On</p>
          </div>

          {/* Form */}
          <div className="px-8 py-6">
            {errors.general && (
              <div className="mb-6 p-4 bg-red-50 border border-red-200 rounded-lg">
                <p className="text-sm text-red-800">{errors.general}</p>
              </div>
            )}

            <form onSubmit={handleSubmit} className="space-y-6">
              <div>
                <Input
                  label="User ID"
                  type="text"
                  value={formData.userId}
                  onChange={(e) => {
                    setFormData({ ...formData, userId: e.target.value.toUpperCase() });
                    setErrors({ ...errors, userId: undefined, general: undefined });
                  }}
                  placeholder="Enter User ID"
                  disabled={loading}
                  required
                  autoFocus
                  maxLength={8}
                  error={errors.userId}
                />
              </div>

              <div>
                <Input
                  label="Password"
                  type="password"
                  value={formData.password}
                  onChange={(e) => {
                    setFormData({ ...formData, password: e.target.value.toUpperCase() });
                    setErrors({ ...errors, password: undefined, general: undefined });
                  }}
                  placeholder="Enter Password"
                  disabled={loading}
                  required
                  maxLength={8}
                  error={errors.password}
                />
              </div>

              <div className="pt-2">
                <Button type="submit" disabled={loading} className="w-full">
                  {loading ? (
                    <span className="flex items-center justify-center">
                      <svg className="animate-spin -ml-1 mr-3 h-5 w-5 text-white" fill="none" viewBox="0 0 24 24">
                        <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4" />
                        <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z" />
                      </svg>
                      Signing in...
                    </span>
                  ) : (
                    'Sign In (Enter)'
                  )}
                </Button>
              </div>
            </form>

            <div className="mt-6 text-center">
              <p className="text-sm text-gray-600">
                Press <kbd className="px-2 py-1 bg-gray-100 border border-gray-300 rounded text-xs">PF3</kbd> to exit
              </p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
