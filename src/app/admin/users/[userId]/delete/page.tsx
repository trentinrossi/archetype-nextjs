'use client';

import React, { useState, useEffect } from 'react';
import { useRouter, useParams } from 'next/navigation';
import { useAuth } from '@/contexts/AuthContext';
import { userService } from '@/services/userService';
import { Button } from '@/components/ui';
import { User } from '@/types/user';

export default function UserDeletePage() {
  const router = useRouter();
  const params = useParams();
  const userId = params.userId as string;
  const { isAuthenticated, isAdmin } = useAuth();
  const [user, setUser] = useState<User | null>(null);
  const [error, setError] = useState('');
  const [successMessage, setSuccessMessage] = useState('');
  const [loading, setLoading] = useState(false);
  const [loadingUser, setLoadingUser] = useState(true);
  const [currentDateTime, setCurrentDateTime] = useState('');

  useEffect(() => {
    if (!isAuthenticated || !isAdmin()) {
      router.push('/login');
      return;
    }
  }, [isAuthenticated, isAdmin, router]);

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
    if (userId) {
      fetchUser();
    }
  }, [userId]);

  const fetchUser = async () => {
    try {
      setLoadingUser(true);
      const userData = await userService.getUserById(userId);
      setUser(userData);
      setSuccessMessage('Press PF5 key to delete this user ...');
    } catch (err) {
      setError(err instanceof Error ? err.message : 'User ID NOT found...');
    } finally {
      setLoadingUser(false);
    }
  };

  const handleDelete = async () => {
    if (!window.confirm(`Are you sure you want to delete user ${userId}?`)) {
      return;
    }

    try {
      setLoading(true);
      setError('');
      setSuccessMessage('');

      const response = await userService.deleteUser(userId);
      setSuccessMessage(response.message);
      setUser(null);
      
      // Redirect after 2 seconds
      setTimeout(() => {
        router.push('/admin/users');
      }, 2000);
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Failed to delete user';
      setError(errorMessage);
    } finally {
      setLoading(false);
    }
  };

  if (loadingUser) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-gray-50">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto"></div>
          <p className="mt-4 text-gray-600">Loading user...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50">
      <div className="bg-gradient-to-r from-red-600 to-pink-600 text-white shadow-lg">
        <div className="max-w-7xl mx-auto px-4 py-4">
          <div className="flex justify-between items-center">
            <div>
              <h1 className="text-2xl font-bold">CU03 - COUSR03C</h1>
              <p className="text-red-100 text-sm">User Delete (Security)</p>
            </div>
            <p className="text-sm">{currentDateTime}</p>
          </div>
        </div>
      </div>

      <div className="max-w-2xl mx-auto px-4 py-6">
        <div className="bg-white rounded-lg shadow-lg p-6">
          {successMessage && !user && (
            <div className="mb-6 p-4 bg-green-50 border border-green-200 rounded-lg text-green-800">
              {successMessage}
            </div>
          )}

          {successMessage && user && (
            <div className="mb-6 p-4 bg-yellow-50 border border-yellow-200 rounded-lg text-yellow-800">
              {successMessage}
            </div>
          )}

          {error && (
            <div className="mb-6 p-4 bg-red-50 border border-red-200 rounded-lg text-red-800">
              {error}
            </div>
          )}

          {user && (
            <>
              <div className="mb-6 p-6 bg-red-50 border-2 border-red-200 rounded-lg">
                <h3 className="text-lg font-semibold text-red-900 mb-4">User Information</h3>
                <div className="space-y-3">
                  <div className="flex justify-between">
                    <span className="font-medium text-gray-700">User ID:</span>
                    <span className="text-gray-900">{user.userId}</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="font-medium text-gray-700">First Name:</span>
                    <span className="text-gray-900">{user.firstName}</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="font-medium text-gray-700">Last Name:</span>
                    <span className="text-gray-900">{user.lastName}</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="font-medium text-gray-700">User Type:</span>
                    <span className={`px-2 py-1 rounded-full text-xs font-semibold ${user.userType === 'A' ? 'bg-purple-100 text-purple-800' : 'bg-green-100 text-green-800'}`}>
                      {user.userType === 'A' ? 'Administrator' : 'Regular User'}
                    </span>
                  </div>
                </div>
              </div>

              <div className="mb-6 p-4 bg-yellow-50 border border-yellow-200 rounded-lg">
                <div className="flex items-start">
                  <svg className="w-6 h-6 text-yellow-600 mr-3 flex-shrink-0" fill="currentColor" viewBox="0 0 20 20">
                    <path fillRule="evenodd" d="M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z" clipRule="evenodd" />
                  </svg>
                  <div>
                    <h4 className="text-sm font-semibold text-yellow-900">Warning</h4>
                    <p className="text-sm text-yellow-800 mt-1">
                      This action cannot be undone. The user will be permanently deleted from the system.
                    </p>
                  </div>
                </div>
              </div>

              <div className="flex gap-4">
                <Button
                  variant="danger"
                  onClick={handleDelete}
                  disabled={loading}
                  className="flex-1"
                >
                  {loading ? 'Deleting User...' : 'Delete User (PF5)'}
                </Button>
              </div>
            </>
          )}

          <div className="mt-6 pt-6 border-t border-gray-200 flex gap-4">
            <Button variant="secondary" onClick={() => router.push('/admin/users')}>
              PF3 - Return to List
            </Button>
            <Button variant="secondary" onClick={() => router.push('/admin')}>
              PF12 - Return to Menu
            </Button>
          </div>
        </div>
      </div>
    </div>
  );
}
